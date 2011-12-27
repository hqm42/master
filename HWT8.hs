{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module HWT8 where

import Network.Wai (Request)
import qualified Control.Monad.State as MS
import Control.Monad.Writer (Writer,WriterT,tell,runWriter,runWriterT,execWriterT)
import Control.Monad.Reader (ReaderT(..),ask,runReaderT)
import Control.Monad (liftM,when)
import Control.Arrow
import Data.String.Utils (join)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Network.HTTP.Types (statusOK)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Data.Monoid
import qualified Data.Map as M
import Data.Dynamic
import System.Random

import Text.JSON.Generic
import Text.JSON.Pretty

import Yesod
import Yesod.Static

import Network.Wai
import Network.Wai.Handler.Warp (run )

import Data.Enumerator.List (consume)

import qualified GDiff as GD

import DefaultApp

-- handle for models + components + Values
newtype HWTId a = HWTId String deriving (Eq,Ord,Typeable,Data)

instance Show (HWTId a) where
  show (HWTId s) = s

-- HWTId generation/conversion
toIntId :: HWTId a -> Int
toIntId (HWTId x) = read $ tail x

type VarDef = HWTId Def
type HWTWidget = HWTId Ele
type Model a accesstype = HWTId (Mod,a,accesstype)
type RWModel a = Model a RW
type RModel a = Readable at => Model a at
type WModel a = Writeable at => Model a at
type Value a location = HWTId (Val,a,location)
type GValue location = Value JSValue location


-- Tags
data Def = Def -- JS Valriable Definition
data Ele = Ele -- Widget/Element
data Mod = Mod -- Model
data Val = Val -- Value
data Act = Act -- Action


-- access types
class Readable at
class Writeable at
data R = R
instance Readable R
data W = W
instance Writeable W
data RW = RW
instance Readable RW
instance Writeable RW

-- Var locations
data S = S -- server
data C = C -- client
data T = T -- transient (client)
data P = P -- polling handler

-- JavaScript Statement -- var foo1243 = new Bla()
type JSS = (HWTId Def, String)

jsValue :: JSS -> String
jsValue = snd

jsReference :: HWTId a -> String
jsReference (HWTId x) = x

-- result of HWT Monad
data InitState = InitState {
    rootNode :: HWTWidget,
    varDefs :: [JSS],
    initValues :: DMap,
    initServerValues :: DMap,
    nextId :: Int -- server value id
  } deriving Show

-- Application State
data HWTApp = HWTApp {
  getStaticFiles :: Static,
  initState :: InitState, -- init page
  sessions :: TVar (M.Map SessionId HWTSession),
  globals :: HWTGlobal,
  listeners :: M.Map (GValue C) [ValueListener]
}

type DMap = GD.DMap Dynamic JSValue

-- global values
type HWTGlobal = TVar DMap

-- per user values
data HWTSession = HWTSession {
  sessionValues :: TVar DMap,
  updates :: TMVar (M.Map (GValue P) JSValue)
}

type SessionId = String


data HWTInit = HWTInit {
  initNextId :: Int,
  hwtVarDefs :: [JSS],
  clientValues :: DMap,
  serverValues :: DMap,
  valueListeners :: M.Map (GValue C) [ValueListener] -- for clientValues only
} deriving (Show)

emptyHWTInit = HWTInit {
  initNextId = 0,
  hwtVarDefs = [],
  clientValues = GD.empty,
  serverValues = GD.empty,
  valueListeners = M.empty -- for clientValues only
}

data ValueListener = ValueListener {
  call :: JSValue -> HWTAction ()
}

instance Show ValueListener where
  show _ = "ValueListener"

type HWT a = MS.State HWTInit a

data ActionScope = ActionScope {
  actionSession :: HWTSession,
  actionGlobal :: HWTGlobal
}

data ValueUpdate = SVU (GValue S) JSValue
                 | CVU (GValue C) JSValue
                 | TVU (GValue T) JSValue

type HWTAction a = WriterT [ValueUpdate] (ReaderT ActionScope STM) a

-- Client

gId :: Value a at -> GValue at
gId (HWTId x) = HWTId x

gpId :: Value a at -> GValue P
gpId (HWTId x) = HWTId x

value :: Data a => a -> HWT (Value a C)
value c = do
  ini@HWTInit{clientValues=cv} <- MS.get
  let
    (GD.Ref gi,cv') = GD.insert c cv
    ini' = ini{clientValues=cv'}
    i = HWTId $ "clientValue" ++ show gi 
    res = (i,"new hwt.Value(pollingHandler," ++ (render $ pp_value $ toJSON c) ++ ",'" ++ (show i) ++ "')")
  MS.put ini'
  defVar res
  return i

transientValue :: Data a => a -> HWT (Value a T)
transientValue c = do
  i <- genId "v"
  let
    res = (i,"new hwt.TransientValue(pollingHandler," ++ (render $ pp_value $ toJSON c) ++ ",'" ++ (show i) ++ "')")
  -- tell $ mempty{clientValues = M.singleton i $ toJSON c} -- TransientValues are not present at server side
  defVar res
  return i

readModel :: Value a at -> HWT (Model a R)
readModel v = addDef ("new hwt.ReadModel(" ++ (jsReference v) ++ ")") "rm"

readWriteModel :: Value a at -> HWT (Model a RW)
readWriteModel v = addDef ("new hwt.ReadWriteModel(" ++ jsReference v ++ ")") "rwm"

negateModel :: Model Bool a -> HWT (Model Bool a)
negateModel m = addDef("new hwt.NegateModel(" ++ jsReference m ++ ")") "nm"

constModel :: Data a => a -> HWT (Model a R)
constModel x = do
  v <- transientValue x
  readModel v

projection :: (a -> b) -> HWTId (Model a c) -> HWT (HWTId (Model b c))
projection p model =  undefined

label :: Show a
      => RModel a 
      -> Maybe (RModel String)
      -> HWT HWTWidget
label s opt_class = addDef (concat ["new hwt.widgets.Label(",opt,jsReference s,")"]) "l"
  where
    opt = case opt_class of
      Nothing -> ""
      Just c -> "," ++ (jsReference c)

textField :: RWModel String 
          -> Maybe (RModel Bool)
          ->  Maybe (RModel String)
          -> HWT HWTWidget
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

panel :: Maybe (RModel Bool) -> Maybe (RModel String) -> [HWTWidget] -> HWT HWTWidget
panel opt_visible opt_class childWidgets = addDef ("new hwt.widgets.Panel(" ++ optv ++ optc ++ "new Array(" ++ ws ++ "))") "p"
  where
    optv = case opt_visible of
      Nothing -> "null,"
      Just m -> (jsReference m) ++ ","
    optc = case opt_class of
      Nothing -> "null,"
      Just m -> (jsReference m) ++ ","
    ws = join "," (map jsReference childWidgets)

gAction :: Data a => (a -> HWTAction ()) -> (JSValue -> HWTAction ())
gAction f = f . uFromJSON

uFromJSON :: Data a => JSValue -> a
uFromJSON = (\(Ok x) -> x) . fromJSON

button :: (Data a, Monoid a, Show c)
       => RModel c
       -> (a -> HWTAction ())
       -> (Value a at)
       -> HWT HWTWidget
button content action actionValue = do
  actionHandlerValue <- value mempty
  valueListener actionHandlerValue $ gAction action
  addDef ("new hwt.widgets.Button(" ++ (jsReference content) 
                                    ++ ",copyTo(" ++ (jsReference actionHandlerValue) ++ "),"
                                    ++ (jsReference actionValue) ++ ")") "b"

-- Client helper

defVar :: (HWTId a,String) -> HWT ()
defVar newVarDef = MS.modify $ \ini@HWTInit{hwtVarDefs=vd} -> ini{hwtVarDefs = vd ++ [newVarDefG]}
  where
    newVarDefG = first (\(HWTId x) -> HWTId x) $ newVarDef

genId :: String -> HWT (HWTId a)
genId s = do
  ini@HWTInit{initNextId=i} <- MS.get
  MS.put ini{initNextId=i+1}
  return $ HWTId $ s ++ (show i)

addDef :: String -> String -> HWT (HWTId a)
addDef c t = do
  i <- genId t
  let
    res = (i,c)
  defVar res
  return i

renderJS :: HWTWidget -> [JSS] -> DMap -> String
renderJS root defs values = join "\n" $ defs' {- ++ inits -} ++ [root'] ++ [poll]
  where
    defs' = map (\(r,v) -> concat ["var ",show r," = ",v,";"]) defs
    root' = concat ["document.body.appendChild(",(jsReference root),".domNode);"]
--  inits = map (\(r,v) -> concat [show r,".init(",show $ pp_value v,");"]) $ GD.toList values
    poll = "pollingHandler.poll();"

-- Server

serverValue :: Data a => a -> HWT (Value a S)
serverValue c = do
  ini@HWTInit{serverValues=sv} <- MS.get
  let
    (GD.Ref gi,sv') = GD.insert c sv
    ini' = ini{serverValues=sv'}
    i = HWTId $ "serverValue" ++ show gi 
    res = (i,"new hwt.ServerValue(pollingHandler,'" ++ (show i) ++ "')")
  MS.put ini'
  defVar res
  return i

valueListener :: Value a C -> (JSValue -> HWTAction ()) -> HWT ()
valueListener v f = do
  ini@HWTInit{valueListeners=vl} <- MS.get
  let
    f' = ValueListener f
    vl' = M.unionWith (++) vl $ M.singleton (gId v) [f']
    ini' = ini{valueListeners=vl'}
  MS.put ini'

-- App

toInitState :: HWT HWTWidget -> (InitState,M.Map (GValue C) [ValueListener])
toInitState e = (InitState root d c s nextId,l)
  where
    (root,inits) = MS.runState e emptyHWTInit
    HWTInit nextId d c s l = inits

runHWTApp :: HWT HWTWidget -> IO ()
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
          initValuesT <- newTVar initValues
          initUpdatesT <- newEmptyTMVar
          writeTVar sessionsT $ M.insert sk (HWTSession initValuesT initUpdatesT) sessions
          return GD.empty -- values are initial and do not have to be initialized again
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
    update (CVU vn vc) = update' (gpId vn) vc (updates session)
    update (TVU vn vc) = update' (gpId vn) vc (updates session)
    update (SVU vn vc) = mapM_ (update' (gpId vn) vc) allSessions
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
  v <- liftIO $ atomically $ evalHWTAction (getValueJSON $ HWTId valueName) (ActionScope s g)
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

getValueJSON :: GValue loc -> HWTAction JSValue
getValueJSON vn = do
  let
    gi = (read $ jsReference vn) 
  ActionScope s g <- ask
  lift $ lift $ do
    svs <- readTVar $ sessionValues s
    if GD.member gi svs then
      return $ GD.render gi svs
    else do
      gvs <- readTVar g
      if GD.member gi gvs then
        return $ GD.render gi gvs
      else
        error $ "Value " ++ (show vn) ++ " not found! session: " ++ show svs ++ " global: " ++ show gvs


getValue :: Data a => Value a loc -> HWTAction a
getValue vn = do
  jv <- getValueJSON $ gId vn
  return $ uFromJSON jv

setValue :: Data a => (Value a loc) -> a -> HWTAction ()
setValue vn newContent = do
  let
    vn' = toIntId vn
    newContent' = toJSON newContent
  ActionScope s g <- ask
  update <- lift $ lift $ do
    gvs <- readTVar g
    if GD.member vn' gvs 
      then
        do
          writeTVar g $ GD.update (GD.Ref vn') newContent gvs
          return $ SVU vn' newContent'
      else
        do
          svs <- readTVar $ sessionValues s
          when (M.member vn' svs) $ do
            writeTVar (sessionValues s) $ M.insert vn' newContent' svs
          return $ CVU vn' newContent'
  tell [update]
  return ()
  
getListeners :: GValue C -> Handler [ValueListener]
getListeners valueId = do
  HWTApp{listeners=ls} <- getYesod
  return $ case M.lookup valueId ls of
    Nothing -> []
    Just lls -> lls

updateSessionValue :: HWTSession -> GValue C -> JSValue -> STM ()
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
