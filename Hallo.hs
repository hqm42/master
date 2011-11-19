
import HWT8

main = runHWTApp $ do
  v1 <- value "Hallo"
  m1 <- readModel v1
  m2 <- readWriteModel v1
  t1 <- textField m2 Nothing Nothing
  l1 <- label m1
  panel Nothing [t1,l1]
