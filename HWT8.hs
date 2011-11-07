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

import Network.Wai
import Network.Wai.Handler.Warp (run )

import Data.Enumerator.List (consume)

import DefaultApp

data HWT8 = HWT8 HWTApp

data JS a = JS {
    value :: String,
    reference :: String
  } deriving Show

data JSS = JSS {
    nextId :: Int,
    varDefs :: [JS VarDef],
    actions :: [(String,ActionH)],
    initialModels :: [(String,String)]
  } -- deriving Show -- actions breaks Show

data HWTApp = HWTApp {
    static :: (String,JS Element,JSS), -- init page
    handlerFunctions :: [(String,ActionH)],
    models :: TVar [(String, TVar [(String,String)])] -- [(sessionKey,[(ref,model)])]
  }

data ActionH = ActionH {
  modelRef :: String,
  f :: (String -> IO String)
}

data VarDef
data Element
data Action

type Elem = JS Element

type HWT a = StateT JSS IO a

container :: [JS Element] -> HWT (JS Element)
container cs = addDef ("mkContainer(" ++ (join "," (map reference cs)) ++ ")") "c"

label :: String -> HWT (JS Element)
label s = do
  l <- addDef ("mkLabel('" ++ s ++ "')") "l"
  addModel l s

button :: String -> (JS Action) -> HWT (JS Element)
button s a = do
  b <- addDef ("mkButton('" ++ s ++ "'," ++ (reference a) ++ ")") "b"
  addModel b s

action :: (JS Element) -> (String -> IO String) -> HWT (JS Action)
action e f = do
  i <- genId "a"
  let
    res = JS ("mkAction(" ++ (reference e) ++ ",'" ++ i ++ "')") i
  defVar [toVarDef res]
  modify (\s -> s{actions = (i,(ActionH (reference e) f)) : (actions s)})
  return res

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

addModel :: JS Element -> String -> HWT (JS Element)
addModel e m = do
  modify (\s -> s{initialModels = ((reference e),m) : (initialModels s)})
  return e

renderJS :: JS Element -> [JS VarDef] -> [(String,String)] -> String
renderJS root defs models = join "\n" $ defs' ++ models' ++ [root']
  where
    defs' = map (\(JS v r) -> concat ["var ",r," = ",v,";"]) defs
    models' = map (\(r,m) -> concat ["setModel(",r,",",show m,");"]) models
    root' = concat ["document.body.appendChild(",(reference root),");"]

runHWTApp :: HWT Elem -> IO ()
runHWTApp e = do
  staticJS <- liftIO $ readFile "hwt8.js"
  (root,jss) <- runStateT e (JSS 0 [] [] [])
  ms <- newTVarIO []
  let
    as = actions jss
  app <- toWaiApp $ HWT8 $ HWTApp (staticJS,root,jss) as ms
  run 8080 $ app
  
-- Yesod routes

mkYesod "HWT8" [parseRoutes|
/ HomeR GET
/action/#String ActionR POST
|]

instance Yesod HWT8 where
    approot _ = ""

getHomeR :: Handler RepHtml
getHomeR = do
  sk <- getSessionKey
  HWT8 (HWTApp (staticJS,root,jss) _ ms) <- getYesod
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
    body = defaultJSApp staticJS (renderJS root (varDefs jss) models)
  liftIO $ putStrLn $ "TVar models: " ++ (show models)
  return $ RepHtml $ toContent body

postActionR :: String -> Handler RepPlain
postActionR actionIndex = do
  sk <- getSessionKey
  HWT8 (HWTApp _ as models) <- getYesod
  case lookup actionIndex as of
    (Just (ActionH mr f)) -> do
      bss <- lift consume
      newModel <- liftIO $ f $ L.unpack $ L.fromChunks bss
      liftIO $ atomically $ do
        updateModel models sk mr newModel
      return $ RepPlain $ toContent newModel
    Nothing -> return $ RepPlain $ toContent ("ERROR" :: String)

updateModel :: TVar [(String, TVar [(String,String)])] -> String -> String -> String -> STM ()
updateModel models sk mr newModel = do
  ms <- readTVar models
  case lookup sk ms of
    Nothing -> undefined -- TODO FIXME
    Just msT -> do
      ms' <- readTVar msT
      writeTVar msT $ (mr,newModel) : [p | p@(r,_) <- ms', r /= mr]
      
  

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
