module Fortune where

import HWT8
import System.Cmd.Utils

main = runHWTApp $ do
  l <- label "Hallo"
  a1 <- action l $ const $ do
    (pid1, x1) <- pipeFrom "fortune" []
    forceSuccess pid1
    return x1
  b1 <- button "fortune" a1
  a2 <- action l $ const $ return "Hallo"
  b2 <- button "reset" a2
  container [b1,b2,l]


