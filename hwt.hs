{-# LANGUAGE NoMonomorphismRestriction #-}
module HWT where

import Network.Wai (Request)

import DefaultApp
import Server

main :: IO ()
main = runServer myApp

myApp :: Request -> String
myApp _req = jsApp
    where
      jsApp = defaultJSApp $ unJS simpleDOM 0
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
