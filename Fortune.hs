module Fortune where

import HWT8
import System.Cmd.Utils

main = runHWTApp $ do
  l <- label "Hallo"
  a <- action l $ \s -> do
    (pid1, x1) <- pipeFrom "fortune" []
    forceSuccess pid1
    return x1
  b <- button "!!!" a
  container [l,b]


