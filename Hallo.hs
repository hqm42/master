module Hallo where

import HWT8

main = runHWTApp $ do
  l <- label "Hallo"
  a <- action l (++ " HALLO")
  b <- button "!!!" a
  container [l,b]
