
import HWT8
import Text.JSON.Generic

j2s :: JSValue -> String
j2s jsv = s
  where
    Ok s = fromJSON jsv

main = runHWTApp $ do
  v1 <- transientValue ""
  sv1 <- serverValue ""
  m1 <- readModel v1
  m2 <- readWriteModel v1
  t1 <- textField m2 Nothing Nothing
  b1 <- button m1 (\s -> do
                          log <- getValue sv1
                          setValue sv1 (toJSON $ (j2s log) ++ ("\n" :: String) ++ (j2s s))) v1
  m3 <- readModel sv1
  l2 <- label m3 Nothing
  panel Nothing [l2,t1,b1]
