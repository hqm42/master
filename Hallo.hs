
import HWT8
import Text.JSON.Generic

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
                          setValue msgCounter $ cnt + 1
                          setValue message "") message
  chatLogOutput <- label chatLogModel Nothing
  panel Nothing [messageInput,sendButton,chatLogOutput]
