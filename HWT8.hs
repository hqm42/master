{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HWT8 where

import Network.Wai (Request)
import qualified Control.Monad.State as MS (StateT(..), get, modify)
import Control.Monad.Writer (Writer,WriterT,tell,runWriter,runWriterT,execWriterT)
import Control.Monad.Reader (ReaderT(..),ask,runReaderT)
import Control.Monad (liftM)
import Data.String.Utils (join)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Network.HTTP.Types (statusOK)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Data.Monoid
import qualified Data.Map as M

import System.Random

import Text.JSON.Generic
import Text.JSON.Pretty

import Yesod
import Yesod.Static

import Network.Wai
import Network.Wai.Handler.Warp (run )

import Data.Enumerator.List (consume)

import DefaultApp

newtype HWTId a = HWTId String deriving (Eq,Ord,Typeable,Data)

instance Show (HWTId a) where
  show (HWTId s) = s

type JS a = (HWTId a,String)

jsValue :: JS a -> String
jsValue = snd

jsReference :: JS a -> String
jsReference = (\(HWTId s) -> s) . fst

data InitState = InitState {
    rootNode :: JS Element,
    varDefs :: [JS VarDef],
    initValues :: M.Map (HWTId Value) JSValue,
    initServerValues :: M.Map (HWTId Value) JSValue,
    nextId :: Int
  } deriving Show

data HWTApp = HWTApp {
  getStaticFiles :: Static,
  initState :: InitState, -- init page
  sessions :: TVar (M.Map SessionId HWTSession),
  globals :: TVar ValueMap,
  listeners :: M.Map (HWTId Value) [ValueListener]
}

data HWTSession = HWTSession {
  sessionValues :: TVar ValueMap,
  updates :: TMVar ValueMap
}

data ValueUpdate = SessionValueUpdate (HWTId Value) JSValue
                 | GlobalValueUpdate (HWTId Value) JSValue

type ValueMap = M.Map (HWTId Value) JSValue
type SessionId = String

data VarDef = VarDef 
data Element = Element
data Model = Model
data Value = Value deriving (Typeable,Data)
data Action = Action

data HWTInit = HWTInit {
  hwtVarDefs :: [JS VarDef],
  clientValues :: ValueMap,
  serverValues :: ValueMap,
  valueListeners :: M.Map (HWTId Value) [ValueListener] -- for clientValues only
} deriving (Show)

data ValueListener = ValueListener {
  call :: JSValue -> HWTAction ()
}

instance Show ValueListener where
  show _ = "ValueListener"

instance Monoid HWTInit where
  mempty = HWTInit {
    hwtVarDefs = [],
    clientValues = M.empty,
    serverValues = M.empty,
    valueListeners = M.empty
  }
  mappend (HWTInit v c s l) (HWTInit v' c' s' l') =
    HWTInit (v `mappend` v')
      (c' `mappend` c) -- reverse append maps -> new Values override old ones
      (s' `mappend` s)
      (M.unionWith mappend l l') -- one Value can have multiple ValueListeners

type HWT a = MS.StateT Int (Writer HWTInit) a

data ActionScope = ActionScope {
  actionSession :: HWTSession,
  actionGlobal :: TVar ValueMap
}

type HWTAction a = WriterT [ValueUpdate] (ReaderT ActionScope STM) a

-- Client

value :: String -> HWT (JS Value)
value c = do
  i <- genId "v"
  let
    res = (i,"new hwt.Value(pollingHandler,'" ++ c ++ "','" ++ (show i) ++ "')")
  tell $ mempty{clientValues = M.singleton i $ toJSON c}
  defVar $ toVarDef res
  return res

transientValue :: String -> HWT (JS Value)
transientValue c = do
  i <- genId "v"
  let
    res = (i,"new hwt.TransientValue('" ++ c ++ "')")
  -- tell $ mempty{clientValues = M.singleton i $ toJSON c} -- TransientValues are not present at server side
  defVar $ toVarDef res
  return res

readModel :: JS Value -> HWT (JS Model)
readModel v = addDef ("new hwt.ReadModel(" ++ (jsReference v) ++ ")") "rm"

readWriteModel :: JS Value -> HWT (JS Model)
readWriteModel v = addDef ("new hwt.ReadWriteModel(" ++ jsReference v ++ ")") "rwm"

label :: JS Model -> Maybe (JS Model) -> HWT (JS Element)
label s opt_class = addDef (concat ["new hwt.widgets.Label(",opt,jsReference s,")"]) "l"
  where
    opt = case opt_class of
      Nothing -> ""
      Just c -> "," ++ (jsReference c)

textField :: JS Model -> Maybe (JS Model) ->  Maybe (JS Model) -> HWT (JS Element)
textField s opt_dis opt_class = do
  let
    dis = case opt_dis of
      Nothing -> ",null"
      Just d -> "," ++ jsReference d
    clas = case opt_class of
      Nothing -> ",null"
      Just c -> "," ++ jsReference c
    opts = dis ++ clas
  addDef ("new hwt.widgets.TextField(" ++ jsReference s ++ opts ++ ")") "t"

panel :: Maybe (JS Model) -> [JS Element] -> HWT (JS Element)
panel opt_class childWidgets = addDef ("new hwt.widgets.Panel(" ++ opt ++ ws ++ ")") "p"
  where
    opt = case opt_class of
      Nothing -> ""
      Just m -> (jsReference m) ++ ","
    ws = join "," (map jsReference childWidgets)

button :: JS Model -> (JSValue -> HWTAction ()) -> JS Value -> HWT (JS Element)
button content action actionValue = do
  actionHandlerValue <- value ""
  valueListener actionHandlerValue action
  addDef ("new hwt.widgets.Button(" ++ (jsReference content) 
                                    ++ ",copyTo(" ++ (jsReference actionHandlerValue) ++ "),"
                                    ++ (jsReference actionValue) ++ ")") "b"

-- Client helper

toVarDef :: JS a -> JS VarDef
toVarDef (HWTId s,v) = (HWTId s,v) 

defVar :: JS VarDef -> HWT ()
defVar newVarDef = tell $ mempty{hwtVarDefs = [newVarDef]}

genId :: String -> HWT (HWTId a)
genId s = do
  i <- MS.get
  MS.modify (+1)
  return $ HWTId $ s ++ (show i)

addDef :: String -> String -> HWT (JS a)
addDef c t = do
  i <- genId t
  let
    res = (i,c)
  defVar $ toVarDef res
  return $ res

renderJS :: JS Element -> [JS VarDef] -> ValueMap -> String
renderJS root defs values = join "\n" $ defs' ++ inits ++ [root'] ++ [poll]
  where
    defs' = map (\(r,v) -> concat ["var ",show r," = ",v,";"]) defs
    root' = concat ["document.body.appendChild(",(jsReference root),".domNode);"]
    inits = map (\(r,v) -> concat [show r,".init(",show $ pp_value v,");"]) $ M.toList values
    poll = "pollingHandler.poll();"

-- Server

serverValue :: String -> HWT (JS Value)
serverValue c = do
  i <- genId "sv"
  let
    res = (i,"new hwt.ServerValue(pollingHandler,'" ++ show i ++ "')")
  tell $ mempty{serverValues = M.singleton i $ toJSON c}
  defVar $ toVarDef res
  return res

valueListener :: JS Value -> (JSValue -> HWTAction ()) -> HWT ()
valueListener v f = do
  tell $ mempty{valueListeners = M.singleton (fst v) [ValueListener f]}

-- App

toInitState :: HWT (JS Element) -> (InitState,M.Map (HWTId Value) [ValueListener])
toInitState e = (InitState root d c s nextId,l)
  where
    e' = MS.runStateT e 0
    ((root,nextId),inits) = runWriter e'
    HWTInit d c s l = inits

runHWTApp :: HWT (JS Element) -> IO ()
runHWTApp e = do
  let
    (is,ls) = toInitState $ do
           defVar $ (HWTId "pollingHandler","new hwt.PollingHandler()")
           e
  putStrLn $ show is
  sessions <- newTVarIO M.empty
  global <- newTVarIO $ initServerValues is
  staticFiles <- static "static"
  app <- toWaiApp $ HWTApp staticFiles is sessions global ls
  run 8080 $ app
  
-- Yesod routes

mkYesod "HWTApp" [parseRoutes|
/ HomeR GET
/value/#String ValueR POST
/updates UpdatesR GET
/updates/#String SingleUpdateR GET
/static StaticR Static getStaticFiles
|]

instance Yesod HWTApp where
    approot _ = ""

getHomeR :: Handler RepHtml
getHomeR = do
  sk <- getSessionKey
  HWTApp{ initState = InitState{rootNode=root,varDefs=varDefs,initValues=initValues}
        , sessions  = sessionsT
        , globals   = globalsT} <- getYesod
  values <- liftIO $ atomically $
    do
      sessions <- readTVar sessionsT
      case M.lookup sk sessions of
        Nothing -> do
          initValuesT <- newTVar M.empty
          initUpdatesT <- newEmptyTMVar
          writeTVar sessionsT $ M.insert sk (HWTSession initValuesT initUpdatesT) sessions
          return M.empty -- values are initial and do not have to be initialized again
        Just session -> readTVar $ sessionValues session
  let
    body = defaultJSApp (renderJS root varDefs values)
  return $ RepHtml $ toContent body

runHWTAction :: HWTAction a -> ActionScope -> STM (a,[ValueUpdate])
runHWTAction action scope = runReaderT (runWriterT action) scope

evalHWTAction :: HWTAction a -> ActionScope -> STM a
evalHWTAction action scope = liftM fst $ runHWTAction action scope

execHWTAction :: HWTAction a -> ActionScope -> STM [ValueUpdate]
execHWTAction action scope = liftM snd $ runHWTAction action scope

postValueR :: String -> Handler RepPlain
postValueR valueName = do
  postData <- lift consume
  let
    newValue = toJSON $ L.unpack $ L.fromChunks postData
    valueId = HWTId valueName
  session <- getHWTSession
  HWTApp{sessions=allSessionsT} <- getYesod
  listeners <- getListeners valueId
  actionScope <- getActionScope
  liftIO $ atomically $ do
    updateSessionValue session valueId newValue
    updates' <- mapM (\ValueListener{call=l} -> execHWTAction (l newValue) actionScope) listeners
    let
      updates = concat updates'
    handleUpdates allSessionsT session updates
  return $ RepPlain $ toContent ("OK: " ++ valueName ++ ":"++ (render $ pp_value newValue) :: String)

handleUpdates :: TVar (M.Map SessionId HWTSession) -> HWTSession -> [ValueUpdate] -> STM ()
handleUpdates allSessionsT session us = do
  allSessions' <- readTVar allSessionsT
  let
    allSessions = map (updates . snd) $ M.toList allSessions'
    update (SessionValueUpdate vn vc) = update' vn vc (updates session)
    update (GlobalValueUpdate vn vc) = mapM_ (update' vn vc) allSessions
    update' vn vc s = do
      oldM <- tryTakeTMVar s
      putTMVar s $ case oldM of
        Nothing -> M.singleton vn vc
        Just old -> M.insert vn vc old 
  mapM_ update us

getUpdatesR :: Handler RepJson
getUpdatesR = do
  sid <- getSessionKey
  liftIO $ putStrLn $ show sid
  HWTSession{updates = updatesTM} <- getHWTSession
  updateMap <- liftIO $ atomically $ takeTMVar updatesTM
  let
    updates = render $ pp_object $ map (\(HWTId k,v) -> (k,v)) $ M.toList updateMap
  return $ RepJson $ toContent ("{\"OK\" : true, \"updates\" : " ++ updates ++ "}" :: String)

getSingleUpdateR :: String -> Handler RepJson
getSingleUpdateR valueName = do
  HWTApp{globals=g} <- getYesod
  s <- getHWTSession
  v <- liftIO $ atomically $ evalHWTAction (getValue $ (HWTId valueName,undefined)) (ActionScope s g)
  return $ RepJson $ toContent $ "{\"OK\" : true, \"value\" : " ++ render (pp_value v) ++ "}"

getHWTSession :: Handler HWTSession
getHWTSession = do
  sk <- getSessionKey
  HWTApp{sessions=ssT} <- getYesod
  ss <- liftIO $ atomically $ readTVar ssT
  case M.lookup sk ss of
    Nothing -> error $ "Session " ++ (show sk) ++ " not found"
    Just sessionT -> return sessionT

getActionScope :: Handler ActionScope
getActionScope = do
  s <- getHWTSession
  HWTApp{globals=g} <- getYesod
  return $ ActionScope s g

getValue :: JS Value -> HWTAction JSValue
getValue (vn,_) = do
  ActionScope s g <- ask
  lift $ lift $ do
    svs <- readTVar $ sessionValues s
    case M.lookup vn svs of
      Nothing -> do
        gvs <- readTVar g
        case M.lookup vn gvs of
          Just v -> return v
          Nothing -> error $ "Value " ++ (show vn) ++ " not found!"
      Just v -> return v

setValue :: JS Value -> JSValue -> HWTAction ()
setValue (vn,_) newContent = do
  ActionScope s g <- ask
  update <- lift $ lift $ do
    gvs <- readTVar g
    case M.lookup vn gvs of
      Nothing -> do
        svs <- readTVar $ sessionValues s
        writeTVar (sessionValues s) $ M.insert vn newContent svs
        return $ SessionValueUpdate vn newContent
      Just _ -> do
        writeTVar g $ M.insert vn newContent gvs
        return $ GlobalValueUpdate vn newContent
  tell [update]
  return ()
  
getListeners :: HWTId Value -> Handler [ValueListener]
getListeners valueId = do
  HWTApp{listeners=ls} <- getYesod
  return $ case M.lookup valueId ls of
    Nothing -> []
    Just lls -> lls

updateSessionValue :: HWTSession -> HWTId Value -> JSValue -> STM ()
updateSessionValue s@HWTSession{sessionValues=vsT} valueName valueContent = do
  vs <- readTVar vsT
  let
    newSessionData = M.insert valueName valueContent vs
  writeTVar vsT newSessionData 

-- Yesod client session

sessionKeyKey = "sessionKey"

getSessionKey :: Handler String
getSessionKey = do
  s <- getSession
  sk <- lookupSession sessionKeyKey
  case sk of
    Just key -> return $ T.unpack key
    Nothing -> do
      newSessionId <- liftIO randomIO
      let
        newSessionKey = "session" ++ show (newSessionId :: Int)
      setSession sessionKeyKey $ T.pack newSessionKey 
      return newSessionKey
