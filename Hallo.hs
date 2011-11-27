
import HWT8
import Text.JSON.Generic

j2s :: JSValue -> String
j2s jsv = s
  where
    Ok s = fromJSON jsv

main = runHWTApp $ do
  message <- transientValue ""
  chatLog <- serverValue ""
  msgCounter <- serverValue (0 :: Int)
  messageModel <- readWriteModel message
  chatLogModel <- readModel chatLog
  msgCounterModel <- readModel msgCounter
  messageInput <- textField messageModel Nothing Nothing
  sendButton <- button msgCounterModel (\newMessage -> do
                          oldMessages <- getValue chatLog
                          setValue chatLog $ oldMessages ++ " " ++ newMessage
                          cnt <- getValue msgCounter
                          setValue msgCounter $ cnt + 1) message
  chatLogOutput <- label chatLogModel Nothing
  panel Nothing [messageInput,sendButton,chatLogOutput]
