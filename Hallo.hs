
import HWT8
import Text.JSON.Generic

j2s :: JSValue -> String
j2s jsv = s
  where
    Ok s = fromJSON jsv

main = runHWTApp $ do
  v1 <- transientValue ""
  sv1 <- serverValue ""
  sv2 <- serverValue (0 :: Int)
  m1 <- readModel v1
  m2 <- readWriteModel v1
  m3 <- readModel sv1
  m4 <- readModel sv2
  t1 <- textField m2 Nothing Nothing
  b1 <- button m4 (\newMessage -> do
                          chatLog <- getValue sv1
                          setValue sv1 $ chatLog ++ "\n" ++ newMessage
                          msgCount <- getValue sv2
                          setValue sv2 $ msgCount + 1) v1
  l2 <- label m3 Nothing
  panel Nothing [l2,t1,b1]
