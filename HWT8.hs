{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HWT8 where

import Network.Wai (Request)
import qualified Control.Monad.State as MS (StateT(..), get, modify)
import Control.Monad.Writer (WriterT(..), Writer,tell,runWriter,runWriterT)
import Data.String.Utils (join)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Network.HTTP.Types (statusOK)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Data.Monoid

import Yesod
import Yesod.Static

import Network.Wai
import Network.Wai.Handler.Warp (run )

import Data.Enumerator.List (consume)

import DefaultApp

data JS a = JS {
    jsValue :: String,
    jsReference :: String
  } deriving Show

data InitState = InitState {
    rootNode :: JS Element,
    varDefs :: [JS VarDef],
    initValues :: [ValueInit],
    initServerValues :: [ValueInit],
    nextId :: Int
  } deriving Show

data HWTApp = HWTApp {
  getStaticFiles :: Static,
  initState :: InitState, -- init page
  sessions :: TVar [(SessionId, HWTSession)],
  globals :: TVar ValueList
}

data HWTSession = HWTSession {
  sessionValues :: TVar ValueList,
  updates :: TMVar ValueList
}
type ValueList = [ValueInit]
type SessionId = String

data VarDef = VarDef 
data Element = Element
data Model = Model
data Value = Value

data ValueInit = ValueInit {
  valueName :: String,
  valueContent :: String
} deriving Show
data HWTInit = HWTInit {
  hwtVarDefs :: [JS VarDef],
  clientValues :: [ValueInit],
  serverValues :: [ValueInit]
} deriving (Show)

instance Monoid HWTInit where
  mempty = HWTInit [] [] []
  mappend (HWTInit v c s) (HWTInit v' c' s') =
    HWTInit (v `mappend` v') (c `mappend` c') (s `mappend` s')

type HWT a = MS.StateT Int (Writer HWTInit) a

-- Client

value :: String -> HWT (JS Value)
value c = do
  i <- genId "v"
  let
    res = JS ("new hwt.Value(pollingHandler,'" ++ c ++ "','" ++ i ++ "')") i
  tell $ mempty{clientValues = [ValueInit i c]}
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

-- Client helper

toVarDef :: JS a -> JS VarDef
toVarDef (JS v r) = JS v r

defVar :: JS VarDef -> HWT ()
defVar newVarDef = tell $ mempty{hwtVarDefs = [newVarDef]}

genId :: String -> HWT String
genId s = do
  i <- MS.get
  MS.modify (+1)
  return $ s ++ (show i)

addDef :: String -> String -> HWT (JS a)
addDef c t = do
  i <- genId t
  let
    res = JS c i
  defVar $ toVarDef res
  return $ res

renderJS :: JS Element -> [JS VarDef] -> [ValueInit] -> String
renderJS root defs values = join "\n" $ defs' ++ inits ++ [root'] ++ [poll]
  where
    defs' = map (\(JS v r) -> concat ["var ",r," = ",v,";"]) defs
    root' = concat ["document.body.appendChild(",(jsReference root),".domNode);"]
    inits = map (\(ValueInit r v) -> concat [r,".init('",v,"');"]) values
    poll = "pollingHandler.poll();"

-- Server

serverValue :: String -> HWT (JS Value)
serverValue c = do
  i <- genId "sv"
  let
    res = JS ("new hwt.ServerValue(pollingHandler,'" ++ i ++ "')") i
  tell $ mempty{serverValues = [ValueInit i c]}
  defVar $ toVarDef res
  return res

-- App

toInitState :: HWT (JS Element) -> InitState
toInitState e = InitState root d c s nextId
  where
    e' = MS.runStateT e 0
    ((root,nextId),inits) = runWriter e'
    HWTInit d c s = inits

runHWTApp :: HWT (JS Element) -> IO ()
runHWTApp e = do
  let
    is = toInitState $ do
           defVar $ JS "new hwt.PollingHandler()" "pollingHandler"
           e
  putStrLn $ show is
  sessions <- newTVarIO []
  global <- newTVarIO $ initServerValues is
  staticFiles <- static "static"
  app <- toWaiApp $ HWTApp staticFiles is sessions global
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
      case lookup sk sessions of
        Nothing -> do
          initValuesT <- newTVar []
          initUpdatesT <- newEmptyTMVar
          writeTVar sessionsT $ (sk,HWTSession initValuesT initUpdatesT) : sessions
          return [] -- values are initial and do not have to be initialized again
        Just session -> readTVar $ sessionValues session
  let
    body = defaultJSApp (renderJS root varDefs values)
  return $ RepHtml $ toContent body

postValueR :: String -> Handler RepPlain
postValueR valueName = do
  postData <- lift consume
  let
    newValue = L.unpack $ L.fromChunks postData
  session <- getHWTSession
  liftIO $ atomically $ updateSessionValue session $ ValueInit valueName newValue
  return $ RepPlain $ toContent ("OK: " ++ valueName ++ ":"++ newValue :: String)

getUpdatesR :: Handler RepJson
getUpdatesR = do
  HWTSession{updates = updatesTM} <- getHWTSession
  updateList <- liftIO $ atomically $ takeTMVar updatesTM
  let
    updates = "{" ++ (join "," (map (\(ValueInit k v) -> concat ["\"",k,"\":\"",v,"\""]) updateList)) ++ "}"
  return $ RepJson $ toContent ("{\"OK\" : true, \"updates\" : " ++ updates ++ "}" :: String)

getSingleUpdateR :: String -> Handler RepJson
getSingleUpdateR valueName = do
  HWTApp{globals=g} <- getYesod
  s <- getHWTSession
  valueM <- liftIO $ atomically $ getValue valueName s g
  return $ RepJson $ toContent $ case valueM of
    Nothing -> "{\"OK\" : false}" :: String
    Just v -> "{\"OK\" : true, \"value\" : \"" ++ v ++ "\"}"

getHWTSession :: Handler HWTSession
getHWTSession = do
  sk <- getSessionKey
  HWTApp{sessions=ssT} <- getYesod
  ss <- liftIO $ atomically $ readTVar ssT
  case lookup sk ss of
    Nothing -> error $ "Session " ++ (show sk) ++ " not found"
    Just sessionT -> return sessionT

getValue :: String -> HWTSession -> TVar ValueList -> STM (Maybe String)
getValue vn HWTSession{sessionValues = s} g = do
  svs <- readTVar s
  case lookup vn svs of
    Nothing -> do
      gvs <- readTVar g
      return $ lookup vn gvs
    jv -> return jv
  where
    lookup k [] = Nothing
    lookup k ((ValueInit k' v):_) | k == k' = Just v
    lookup k (_:vis) = lookup k vis

updateSessionValue :: HWTSession -> ValueInit -> STM ()
updateSessionValue s@HWTSession{sessionValues=vsT} vi@(ValueInit valueName valueContent) = do
  vs <- readTVar vsT
  let
    newSessionData = vi : [p | p@(ValueInit r _) <- vs, r /= valueName]
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
      newSessionKey <- newIdent
      setSession sessionKeyKey $ T.pack newSessionKey
      return newSessionKey
