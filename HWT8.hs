{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HWT8 where

import Network.Wai (Request)
import Control.Monad.State (StateT(..), gets, modify)
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

data JSS = JSS {
    nextId :: Int,
    varDefs :: [JS VarDef],
    initialModels :: [(String,String)]
  } -- deriving Show

data HWTApp = HWTApp {
  getStaticFiles :: Static,
  static :: (JS Element,JSS), -- init page
  models :: TVar [(String, TVar [(String,String)])] -- [(sessionKey,[(ref,model)])]
}

data VarDef = VarDef 
data Element = Element
data Model = Model
data Value = Value

type HWT a = StateT JSS IO a

value :: String -> HWT (JS Value)
value c = do
  i <- genId "v"
  let
    res = JS ("new hwt.Value('" ++ c ++ "','" ++ i ++ "')") i
  defVar [toVarDef res]
  return res

readModel :: JS Value -> HWT (JS Model)
readModel v = addDef ("new hwt.ReadModel(" ++ (jsReference v) ++ ")") "rm"

readWriteModel :: JS Value -> HWT (JS Model)
readWriteModel v = addDef ("new hwt.ReadWriteModel(" ++ jsReference v ++ ")") "rwm"

label :: JS Model -> HWT (JS Element)
label s = do
  addDef (concat ["new hwt.widgets.Label(",jsReference s,")"]) "l"

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

defVar newVarDefs = do
  modify (\s -> s{varDefs = (varDefs s) ++ newVarDefs})

genId s = do
  i <- gets nextId
  modify (\s -> s{nextId = i + 1})
  return $ s ++ (show i)

addDef :: String -> String -> HWT (JS a)
addDef c t = do
  i <- genId t
  let
    res = JS c i
  defVar [toVarDef res]
  return $ res

renderJS :: JS Element -> [JS VarDef] -> [(String,String)] -> String
renderJS root defs models = join "\n" $ defs' ++ [root']
  where
    defs' = map (\(JS v r) -> concat ["var ",r," = ",v,";"]) defs
    root' = concat ["document.body.appendChild(",(jsReference root),".domNode);"]

runHWTApp :: HWT (JS Element) -> IO ()
runHWTApp e = do
  (root,jss) <- runStateT e (JSS 0 [] [])
  ms <- newTVarIO []
  staticFiles <- staticDevel "static"
  app <- toWaiApp $ HWTApp staticFiles (root,jss) ms
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
  HWTApp _ (root,jss) ms <- getYesod
  models <- liftIO $ atomically $
    do
      ms' <- readTVar ms
      case lookup sk ms' of
        Nothing -> do
          let
            im = (initialModels jss)
          initialModelsT <- newTVar im
          writeTVar ms $ (sk,initialModelsT) : ms'
          return im
        Just modelsT -> readTVar modelsT
  let
    body = defaultJSApp (renderJS root (varDefs jss) models)
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

updateValues :: TVar [(String, TVar [(String,String)])] -> String -> String -> String -> STM (String,[(String,String)])
updateValues values sk valueName newValue = do
  vs <- readTVar values
  case lookup sk vs of
    Nothing -> undefined -- TODO FIXME : Session not found
    Just vsT -> do
      vs' <- readTVar vsT
      let
        newSessionData = (valueName,newValue) : [p | p@(r,_) <- vs', r /= valueName]
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
