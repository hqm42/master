{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module HWT where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (statusOK)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Char8 (unpack)
import qualified Data.Text.Lazy as T
import Control.Monad.Trans (liftIO)

import DefaultApp

main :: IO ()
main = do
  run 8080 $ myApp

myApp :: Application
myApp req = do
  liftIO $ putStrLn $ show $ pathInfo req
  return $ responseLBS status headers (L.pack $ jsApp)
    where
      status = statusOK
      headers = [("Content-Type", "text/html")]
      init = (unJS simpleDOM 0)
      jsApp = defaultJSApp init
      htmlApp = defaultHTMLApp $ unHTML simpleDOM


-- example

simpleDOM = tag "a" "div" [
              tag "b" "span" [
                text "Hallo Welt!"],
              tag "c" "p" [
                text "lorem ipsum",
                tag "d" "br" [],
                tag "e" "em" [text "aaa"]]]

-- super simple DOM 

class SYM s where
  tag :: String -> String -> [s String] -> s String
  text :: String -> s String

-- javascript

newtype JS a = JS {unJS :: Int -> String}

addEtoParent i = ((indent i) " var parent = parent || document.body;\n") ++
                  (indent i) " parent.appendChild(e);\n"

indent i = (++) (replicate i ' ')

instance SYM JS where
  tag id name children =
    JS $ \i -> let
        ind = indent i
      in
      ind "{var oldP = parent;\n" ++
      ind " var e = document.createElement(\"" ++ name ++ "\");\n" ++
      ind " e.setAttribute(\"id\",\"" ++ id ++ "\");\n" ++
      (addEtoParent i) ++
      ind " parent = e;\n" ++
      ind " " ++ (concat $ map (`unJS` (i+2)) children) ++ "\n" ++
      ind " parent = oldP;};\n"
          
      
  text s = JS $ \i -> (indent i) "{var e = document.createTextNode(\"" ++ s ++ "\");\n" ++
                      (addEtoParent i) ++
                      (indent i) "};"

-- HTML

newtype HTML a = HTML {unHTML :: String}

instance SYM HTML where
  tag id name [] = HTML $ "<" ++ name ++ " id=\"" ++ id ++ "\"/>\n"
  tag id name children =
    HTML $ "<" ++ name ++ " id=\"" ++ id ++ "\">\n"
        ++ (concat $ map unHTML children)
        ++ "</" ++ name ++ ">\n"
  text s = HTML $ s ++ "\n"
