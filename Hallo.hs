
import HWT8
import Control.Monad (when)

joinPanel visible nameOutput = do
  joinName <- transientValue ""
  nameModel <- readWriteModel joinName
  nameField <- textField nameModel Nothing Nothing
  joinText <- constModel "join"
  joinButton <- button joinText (\name -> when (name /= "") $ do
                                  setValue nameOutput name
                                  setValue visible False) joinName
  visibleModel <- readModel visible
  panel (Just visibleModel) Nothing [nameField,joinButton]

chatPanel visibleModel  userName = do
  message <- transientValue ""
  chatLog <- serverValue ""
  msgCounter <- serverValue (0 :: Int)
  messageModel <- readWriteModel message
  chatLogModel <- readModel chatLog
  msgCounterModel <- readModel msgCounter
  messageInput <- textField messageModel Nothing Nothing
  sendButton <- button msgCounterModel (\newMessage -> do
                          uName <- getValue userName
                          oldMessages <- getValue chatLog
                          setValue chatLog $ oldMessages ++ " " ++ uName ++ ": "++ newMessage
                          cnt <- getValue msgCounter
                          setValue msgCounter $ cnt + 1
                          setValue message "") message
  chatLogOutput <- label chatLogModel Nothing
  panel (Just visibleModel) Nothing [messageInput,sendButton,chatLogOutput]

main = runHWTApp $ do
  joinVisible <- transientValue True
  joinVisibleModel <- readModel joinVisible
  chatVisibleModel <- negateModel joinVisibleModel
  userName <- value "HansWurst"
  jp <- joinPanel joinVisible userName
  cp <- chatPanel chatVisibleModel userName
  panel Nothing Nothing [jp,cp]
