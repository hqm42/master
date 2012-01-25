{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HWT.Server where

import HWT.Types
import HWT.Core
import HWT.Session

import qualified Data.GenericDiffMap as GDM

import Yesod
import Yesod.Static
import Network.Wai
import Network.Wai.Handler.Warp (run)

import qualified Data.Map as M
import Data.Monoid
import Control.Monad.State
import Control.Concurrent.AdvSTM
import Control.Concurrent.AdvSTM.TVar
import Control.Concurrent.AdvSTM.TMVar
import Control.Monad.Reader
import Control.Monad.Writer
import System.Random
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L
import Text.JSON.Types
import Text.JSON.Pretty
import Text.JSON.Generic
import qualified Data.String.Utils as SU (join)
import Data.Enumerator.List (consume)


newApplicationState :: (Element,HWTInit) -> IO HWTApplicationState
newApplicationState (e,ini) = do
  svs <- newTVarIO $ initialServerValues ini
  allSes <- newTVarIO M.empty
  staticFiles <- static "static"
  let
    newSession = do
      cvs <- newTVarIO $ initialClientValues ini
      wins <- newTVarIO M.empty
      let
        newWindow = do
          wvs <- newTVarIO $ initialWindowValues ini
          ups <- newEmptyTMVarIO
          let
            win = HWTWindow svs cvs wvs ups allSes wins
          return win
        ses = HWTSession svs cvs wins newWindow
      return ses
    svl = initialServerValueListeners ini
    cvl = initialClientValueListeners ini
    wvl = initialWindowValueListeners ini
    as = HWTApplicationState svs allSes svl cvl wvl newSession (e,constructorCalls ini) staticFiles
  return as

runHWTApp :: Int -> HWT Element -> IO ()
runHWTApp port app = do
  let
    ini = runState app newHWTInit
  -- putStrLn $ show ini
  as <- newApplicationState ini
  waiApp <- toWaiApp as
  run port waiApp

instance Yesod HWTApplicationState where
  approot _ = ""

mkYesod "HWTApplicationState" [parseRoutes|
/ FullPageReloadR GET
/v/#String ValueChangedR POST
/u UpdatesR GET
/u/#String ValueR GET
/static StaticR Static getStaticFiles
|]

window :: Handler HWTWindow
window = do
  as <- getYesod
  s <- session
  wkM <- lookupGetParam "windowKey"
  wk <- case wkM of
    Nothing -> do
      sk <- sessionKey
      windowKey sk
    Just wk -> return $ WK $ T.unpack wk
  ws <- liftIO $ atomically $ readTVar (sessionWindows s)
  case M.lookup wk ws of
    Nothing -> error "window not found \"window\""
    Just w -> return w

session :: Handler HWTSession
session = do
  as <- getYesod
  sk <- sessionKey
  ss <- liftIO $ atomically $ readTVar (applicationSessions as)
  case M.lookup sk ss of
    Nothing -> error "session not found \"session\""
    Just s -> return s

getValueR :: String -> Handler RepJson
getValueR someId = do
  win <- window
  gdmvM <- liftIO $ atomically $ do
    svs <- readTVar $ windowServerValues win
    cvs <- readTVar $ windowClientValues win
    wvs <- readTVar $ windowWindowValues win
    return $ case toHWTId someId :: (Maybe (ServerValue ())) of
      Just (HWTId i _) -> GDM.lookupGDMapValue i svs
      Nothing -> case toHWTId someId :: (Maybe (ClientValue ())) of
        Just (HWTId i _) -> GDM.lookupGDMapValue i cvs
        Nothing -> case toHWTId someId :: (Maybe (WindowValue ())) of
          Just (HWTId i _) -> GDM.lookupGDMapValue i wvs
          Nothing -> Nothing
  case gdmvM of
    Just gdmv -> return $ RepJson $ toContent $ renderJSON $ toJSON $ gdmv
    _ -> error $ "Value '" ++ someId ++ "' not found"
  where
    toJSON (GDM.GDPrimitive json) = json
    toJSON c = complex2JSON c

toMaps :: SessionUpdates
       -> UpdateMaps String
toMaps ups = s (serverValueUpdates ups)
  `mappend`  s (clientValueUpdates ups)
  `mappend`  s (windowValueUpdates ups)
  `mappend`  s (transientValueUpdates ups)
  where
    s :: HWTPrefix (Value () loc) => UpdateMaps (Value () loc) -> UpdateMaps String
    s um = UpdateMaps
      (M.mapKeys show $ newValues um)
      (M.mapKeys show $ changedValues um)
      (map show $ removedValues um)

getUpdatesR :: Handler RepJson
getUpdatesR = do
  w <- window
  ups <- liftIO $ atomically $ takeTMVar (windowUpdates w)
  let
    upsS = toMaps ups
    new = JSObject $ JSONObject $ M.toList $ newValues upsS
    changed = JSObject $ JSONObject $ M.toList $ changedValues upsS
    removed = toJSON $ removedValues upsS
    ups' = [("new",new),("changed",changed),("removed",removed)]
    json = render $ pp_object ups'
  return $ RepJson $ toContent json

runAction :: HWTWindow -> HWTAction () -> IO SessionUpdates
runAction win a = atomically $ runReaderT (execWriterT a) win

postValueChangedR :: String -> Handler RepPlain
postValueChangedR someId = do
  as <- getYesod
  win <- window
  postData <- lift consume
  let
    newContent = case decode $ L.unpack $ L.fromChunks postData of
      Ok c -> c
      Error s -> error s
    setValue' = case toHWTId someId :: (Maybe (ServerValue ())) of
      Just sv -> do
        deserializeValue sv newContent
        callListeners sv as
      Nothing -> case toHWTId someId :: (Maybe (ClientValue ())) of
        Just cv -> do
          deserializeValue cv newContent
          callListeners cv as
        Nothing -> case toHWTId someId :: (Maybe (WindowValue ())) of
          Just wv -> do
            deserializeValue wv newContent
            callListeners wv as
          Nothing -> error $ "Value '" ++ someId ++ "' does not exist! 'postValueChangedR'"
  ups <- liftIO $ atomically $ runReaderT (execWriterT setValue') win
  handleUpdates win ups
  return $ RepPlain $ toContent ("OK" :: String)

updateWindow :: SessionUpdates -> HWTWindow -> AdvSTM ()
updateWindow ups win = do
  let
    updTM = windowUpdates win
  oldUpsM <- tryTakeTMVar updTM
  putTMVar updTM $ case oldUpsM of
    Nothing -> ups
    Just oldUps -> mappend oldUps ups

handleUpdates :: HWTWindow -> SessionUpdates -> Handler ()
handleUpdates win upsW = do
  liftIO $ atomically $ do
    -- set updates for this window:
    updateWindow upsW win
    -- set updates for other windows:
    let
      upsC = upsW{windowValueUpdates=mempty,transientValueUpdates=mempty}
    wins <- readTVar $ windowWindows win
    mapM_ (updateWindow upsC) $ M.elems wins
    -- set updates for other sessions:
    let
      upsS = upsC{clientValueUpdates=mempty}
    allSess <- readTVar $ windowSessions win
    allWins' <- mapM (\s -> readTVar $ sessionWindows s) $ M.elems allSess
    let
      allWins = concat $ map M.elems allWins'
    mapM_ (updateWindow upsS) allWins

defaultJSApp :: String -> String
defaultJSApp jsDyn = "<html>\n<head>\n"
          ++ "<script src='static/closure-library/closure/goog/base.js'></script>"
          ++ "<script src='static/hwt.js'></script>"
          ++ "<script type=\"text/javascript\">\nfunction initHWT() {\n"
          ++ jsDyn
          ++ "\n};\n</script>\n</head>\n<body onload=\"initHWT()\">\n</body>\n</html>"

getFullPageReloadR :: Handler RepHtml
getFullPageReloadR = do
  (sessionKey,windowKey@(WK wk)) <- keys
  as <- getYesod
  w <- windowByKeys sessionKey windowKey
  svs <- liftIO $ atomically $ readTVar $ windowServerValues w
  cvs <- liftIO $ atomically $ readTVar $ windowClientValues w
  let
    prContext = HWTPageReloadContext svs cvs
    (root,constrs) = applicationInit as
    pollingHandler = "var pollingHandler = new hwt.PollingHandler(" ++ show wk ++ ");\n"
    ccalls = concat $ map (\cc -> runReader (renderConstructorCalls cc) prContext) constrs
    insertRootNode = "document.body.appendChild(" ++ show root ++ ".domNode);\n"
    startPolling = "pollingHandler.poll();"
    jsDyn = pollingHandler ++ ccalls ++ insertRootNode ++ startPolling
  return $ RepHtml $ toContent $ defaultJSApp jsDyn

keys :: Handler (SessionKey,WindowKey)
keys = do
  sk <- sessionKey
  wk <- windowKey sk
  return (sk,wk)

sessionKey :: Handler SessionKey
sessionKey = do
  s <- getSession
  sk <- lookupSession "sessionKey"
  case sk of
    Just key -> return $ SK $ T.unpack key
    Nothing -> do
      newSessionId <- liftIO randomIO
      let
        newSessionKey = "session" ++ show (newSessionId :: Int)
        sk = SK newSessionKey
      setSession "sessionKey" $ T.pack newSessionKey 
      insertNewSession sk
      return sk

windowKey :: SessionKey -> Handler WindowKey
windowKey sk = do
  as <- getYesod
  newWindowId <- liftIO randomIO
  ss <- liftIO $ atomically $ readTVar $ applicationSessions as
  let
    sM = M.lookup sk ss 
    newWindowKey = "window" ++ show (newWindowId :: Int)
    wk = WK newWindowKey
  s <- case sM of
    Nothing -> insertNewSession sk
    Just s -> return s
  insertNewWindow s wk
  return wk

windowByKeys :: SessionKey -> WindowKey -> Handler HWTWindow
windowByKeys sk wk = do
  as <- getYesod
  liftIO $ atomically $ do
    ss <- readTVar $ applicationSessions as
    case M.lookup sk ss of
      Nothing -> error "Session not found 'windowBayKeys'"
      Just s -> do
        ws <- readTVar $ sessionWindows s
        case M.lookup wk ws of
          Nothing -> error "Window not found 'windowByKeys'"
          Just w -> return w


insertNewSession :: SessionKey -> Handler HWTSession
insertNewSession sk = do
  as <- getYesod
  s <- liftIO $ newSession as
  liftIO $ atomically $ do
    ss <- readTVar $ applicationSessions as
    writeTVar (applicationSessions as) (M.insert sk s ss)
  return s

insertNewWindow :: HWTSession -> WindowKey -> Handler HWTWindow
insertNewWindow s wk = do
  w <- liftIO $ newWindow s
  liftIO $ atomically $ do
    ws <- readTVar (sessionWindows s)
    writeTVar (sessionWindows s) (M.insert wk w ws)
  return w

