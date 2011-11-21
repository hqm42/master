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
    nextId :: Int
  } deriving Show

data HWTApp = HWTApp {
  getStaticFiles :: Static,
  initState :: InitState, -- init page
  values :: TVar [(String, TVar [ValueInit])] -- [(sessionKey,[(ref,value)])]
}

data VarDef = VarDef 
data Element = Element
data Model = Model
data Value = Value

data ValueInit = ValueInit String String deriving Show

type HWT a = MS.StateT Int (WriterT [JS VarDef] (Writer [ValueInit])) a

value :: String -> HWT (JS Value)
value c = do
  i <- genId "v"
  let
    res = JS ("new hwt.Value('" ++ c ++ "','" ++ i ++ "')") i
  lift $ lift $ tell [ValueInit i c]
  defVar [toVarDef res]
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

toVarDef :: JS a -> JS VarDef
toVarDef (JS v r) = JS v r

defVar newVarDefs = tell newVarDefs

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
  defVar [toVarDef res]
  return $ res

renderJS :: JS Element -> [JS VarDef] -> [ValueInit] -> String
renderJS root defs values = join "\n" $ defs' ++ values' ++ [root']
  where
    defs' = map (\(JS v r) -> concat ["var ",r," = ",v,";"]) defs
    root' = concat ["document.body.appendChild(",(jsReference root),".domNode);"]
    values' = map (\(ValueInit r v) -> concat [r,".init('",v,"');"]) values

toInitState :: HWT (JS Element) -> InitState
toInitState e = InitState root defs inits nextId
  where
    e' = MS.runStateT e 0 -- m (JS Element, newId)
    e'' = runWriterT e' -- m ((JS Element, newId),[JS VarDef])
    (((root,nextId),defs),inits) = runWriter e'' -- (((JS Element, newId),[JS VarDef]),[ValueInit])

runHWTApp :: HWT (JS Element) -> IO ()
runHWTApp e = do
  let
    is = toInitState e
  putStrLn $ show is
  ms <- newTVarIO []
  staticFiles <- staticDevel "static"
  app <- toWaiApp $ HWTApp staticFiles is ms
  run 8080 $ app
  
-- Yesod routes

mkYesod "HWTApp" [parseRoutes|
/ HomeR GET
/value/#String ValueR POST
/static StaticR Static getStaticFiles
|]

instance Yesod HWTApp where
    approot _ = ""

getHomeR :: Handler RepHtml
getHomeR = do
  sk <- getSessionKey
  HWTApp _ InitState{rootNode=root,varDefs=varDefs,initValues=initValues} sessionsT <- getYesod
  values <- liftIO $ atomically $
    do
      sessions <- readTVar sessionsT
      case lookup sk sessions of
        Nothing -> do
          initValuesT <- newTVar []
          writeTVar sessionsT $ (sk,initValuesT) : sessions
          return [] -- values are initial and do not have to be initialized again
        Just valuesT -> readTVar valuesT
  let
    body = defaultJSApp (renderJS root varDefs values)
  return $ RepHtml $ toContent body

postValueR :: String -> Handler RepPlain
postValueR valueName = do
  postData <- lift consume
  let
    newValue = L.unpack $ L.fromChunks postData
  sk <- getSessionKey
  HWTApp _ _ values <- getYesod
  info <- liftIO $ atomically $ do updateValues values sk valueName newValue
  liftIO $ putStrLn $ show info
  return $ RepPlain $ toContent ("OK: " ++ valueName ++ ":"++ newValue :: String)

updateValues :: TVar [(String, TVar [ValueInit])] -> String -> String -> String -> STM (String,[ValueInit])
updateValues values sk valueName newValue = do
  vs <- readTVar values
  case lookup sk vs of
    Nothing -> undefined -- TODO FIXME : Session not found
    Just vsT -> do
      vs' <- readTVar vsT
      let
        newSessionData = (ValueInit valueName newValue) : [p | p@(ValueInit r _) <- vs', r /= valueName]
      writeTVar vsT newSessionData 
      return (sk,newSessionData)
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
