
import HWT8

main = runHWTApp $ do
  v1 <- value "Hallo"
  m1 <- readModel v1
  m2 <- readWriteModel v1
  t1 <- textField m2 Nothing Nothing
  l1 <- label m1 Nothing
  sv1 <- serverValue "asdf"
  m3 <- readModel sv1
  l2 <- label m3 Nothing
  panel Nothing [t1,l1,l2]
