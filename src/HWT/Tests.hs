module HWT.Tests where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer hiding (listen)
import Control.Concurrent.AdvSTM
import Control.Concurrent.AdvSTM.TVar
import Control.Concurrent.AdvSTM.TMVar
import Data.Monoid
import Data.Maybe
import qualified Data.IntMap as IM
import List (sort)

import Data.GenericDiffMap.Tests hiding (test1)
import Data.GenericDiffMap.Projection
import HWT.Types
import HWT.Core
import HWT.Session
import HWT.Server hiding (main)

-- type HWTAction = WriterT SessionUpdates (ReaderT HWTSession AdvSTM)

test :: HWT Element -> IO ()
test = runHWTApp 8080

chat :: HWT Element
chat = do
  loginVisibleV <- clientValue True
  loginVisible <- readModel loginVisibleV
  chatVisible <- negateModel loginVisible
  usernameV <- transientValue ""
  user <- clientValue ""
  usernameM <- readWriteModel usernameV
  nameBox <- textfield usernameM
  loginM <- constModel "login"
  roomV <- windowValue "defaultRoom"
  roomsV <- serverValue ([("defaultRoom",([],[]))] :: [(String,([String],[String]))])
  loginButton <- submitButton loginM usernameV $ \uname -> if uname == [] then
    return ()
    else do
    setValue user uname
    setValue loginVisibleV False
    rooms <- getValue roomsV
    let
      (us,cl)= fromJust $ lookup "defaultRoom" rooms
      defRoom' = ("defaultRoom",(sort $ uname:us,cl))
      rooms' = defRoom' : filter (\(rn,_) -> rn /= "defaultRoom") rooms
    setValue roomsV rooms'
  
  loginPanel <- panel [nameBox,loginButton] (Just loginVisible) Nothing

  msgV <- transientValue ""
  msgM <- readWriteModel msgV
  msgfield <- textfield msgM


  chatlogV <- projection (head_p >=> snd_p >=> snd_p) roomsV
  chatlog <- list chatlogV (\v -> do m <- readModel v
                                     l <- label m
                                     panel [l] Nothing Nothing)

  submitM <- constModel "send"
  sendbutton <- submitButton submitM msgV $ \msg -> do
    roomname <- getValue roomV
    name <- getValue user
    rooms <- getValue roomsV
    let
      msg' = name ++ ": " ++ msg
      rooms' = case lookup roomname rooms of
        Nothing -> (roomname,([name],[msg'])):rooms
        Just (users,log) -> (roomname,(users,take 15 $ msg':log)) : (filter (\(rn,_) -> rn /= roomname) rooms)
    setValue roomsV rooms'
    setValue msgV ""

  roomlist <- list roomsV $ \roomV -> do
    roomNameV <- projection fst_p roomV
    roomNameM <- readModel roomNameV
    roomNameL <- label roomNameM
    panel [roomNameL] Nothing Nothing

  rllabel <- labelC "Available chatrooms:"
  roomListPanel <- panel [rllabel,roomlist] Nothing Nothing

  userlistV <- projection (head_p >=> snd_p >=> fst_p) roomsV
  userlist <- list userlistV $ \unV -> do
                                unM <- readModel unV
                                l <- label unM
                                panel [l] Nothing Nothing
  ullabel <- labelC "Users in this Channel:"
  roomM <- readModel roomV
  roomnamelabel <- label roomM
  userListPanel <- panel [ullabel,userlist] Nothing Nothing
  chatPanel <- panel [roomnamelabel,chatlog,msgfield,sendbutton,userListPanel,roomListPanel] (Just chatVisible) Nothing
  panel [loginPanel,chatPanel] Nothing Nothing

main = test chat
