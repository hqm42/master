
import HWT8

main = runHWTApp $ do
  l <- label "Hallo"
  a <- action l $ (\s -> return $ s ++ " HALLO")
  b <- button "!!!" a
  container [l,b]
