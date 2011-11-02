module HWT8 where

import Network.Wai (Request)
import Control.Monad.State (StateT(..), gets, modify)
import Data.String.Utils (join)

import DefaultApp
import Server

ex = do
  l <- label "Hallo"
  a <- action l (++ "?!")
  b <- button "!!!" a
  container [l,b]

data JS a = JS {
    value :: String,
    reference :: String
  } deriving Show

data JSS = JSS {
    nextId :: Int,
    varDefs :: [JS VarDef]
  } deriving Show

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
action e f = addDefReturn ("mkAction(" ++ (reference e) ++ "," ++ "'ACTIONNAME'" ++ ")") "a"

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

main :: IO ()
main = do
  staticJS <- readFile "hwt8.js"
  s <- (runStateT ex (JSS 0 []))
  let
    (root,jss) = s
  runServer $ \_req -> defaultJSApp staticJS (renderJS root (varDefs jss))
