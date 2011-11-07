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
import Network.HTTP.Types (statusOK)
import Network.Wai

import Yesod

import Network.Wai
import Network.Wai.Handler.Warp (run )

import Data.Enumerator.List (consume)

import DefaultApp
import Server

data HWT8 = HWT8 [(String,(String->String))]

-- BEGIN Hello World EXAMPLE
ex = do
  l <- label "Hallo"
  a <- action l (++ "?!")
  b <- button "!!!" a
  container [l,b]
-- END Hello World EXAMPLE

exEval = runStateT ex (JSS 0 [] [])

data JS a = JS {
    value :: String,
    reference :: String
  } deriving Show

data JSS = JSS {
    nextId :: Int,
    varDefs :: [JS VarDef],
    actions :: [(String,(String->String))]
  } -- deriving Show -- actions breaks Show

data VarDef
data Element
data Action

type HWT a = StateT JSS IO a

container :: [JS Element] -> HWT (JS Element)
container cs = addDefReturn ("mkContainer(" ++ (join "," (map reference cs)) ++ ")") "c"

label :: String -> HWT (JS Element)
label s = addDefReturn ("mkLabel('" ++ s ++ "')") "l" 

button :: String -> (JS Action) -> HWT (JS Element)
button s a = addDefReturn ("mkButton('" ++ s ++ "'," ++ (reference a) ++ ")") "b"

action :: (JS Element) -> (String -> String) -> HWT (JS Action)
action e f = do
  i <- genId "a"
  let
    res = JS ("mkAction(" ++ (reference e) ++ ",'" ++ i ++ "')") i
  defVar [toVarDef res]
  modify (\s -> s{actions = (i,f) : (actions s)})
  return res

toVarDef :: JS a -> JS VarDef
toVarDef (JS v r) = JS v r

defVar newVarDefs = do
  modify (\s -> s{varDefs = (varDefs s) ++ newVarDefs})

genId s = do
  i <- gets nextId
  modify (\s -> s{nextId = i + 1})
  return $ s ++ (show i)

addDefReturn :: String -> String -> HWT (JS a)
addDefReturn c t = do
  i <- genId t
  let
    res = JS c i
  defVar [toVarDef res]
  return $ res

renderJS :: JS Element -> [JS VarDef] -> String
renderJS root defs = join "\n" $ defs' ++ [root']
  where
    defs' = map (\(JS v r) -> concat ["var ",r," = ",v,";"]) defs
    root' = concat ["document.body.appendChild(",(reference root),");"]

main = do
  (_,jss) <- exEval
  let
    as = actions jss
  app <- toWaiApp (HWT8 as)
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
  staticJS <- liftIO $ readFile "hwt8.js"
  (root,jss) <- liftIO $ exEval
  let
    body = defaultJSApp staticJS (renderJS root (varDefs jss))
    status = statusOK
    headers = [("Content-Type", "text/html")]
  return $ RepHtml $ toContent body

postActionR :: String -> Handler RepPlain
postActionR actionIndex = do
  HWT8 as <- getYesod
  case lookup actionIndex as of
    (Just f) -> do bss <- lift consume
                   return $ RepPlain $ toContent $ f $ L.unpack $ L.fromChunks bss
    Nothing -> return $ RepPlain $ toContent ("ERROR" :: String)
  -- return $ RepPlain $ toContent actionIndex
