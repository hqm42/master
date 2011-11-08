module HalloTextfield where

import HWT8

main = runHWTApp $ do
  t <- textField "Hallo"
  a <- action t $ (\s -> return $ if s == "" then "Hallo" else init s)
  b <- button "<-" a
  container [l,b]
