{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HWT.Tests where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer hiding (listen)
import Control.Concurrent.AdvSTM
import Control.Concurrent.AdvSTM.TVar
import Control.Concurrent.AdvSTM.TMVar
import Data.Monoid
import Data.Maybe
import Data.Data
import qualified Data.IntMap as IM
import List (sort)

import Data.GenericDiffMap.Projection
import HWT.Types
import HWT.Core
import HWT.Session
import HWT.Server hiding (main)

-- type HWTAction = WriterT SessionUpdates (ReaderT HWTSession AdvSTM)

test :: HWT Element -> IO ()
test = runHWTApp 8080

assoc :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
assoc key val [] = [(key,val)]
assoc key val (p@(key',_):xs) | key == key' = (key,val):xs
                              | otherwise = p:(assoc key val xs)

toggleModels :: ( HWTPrefix (Value Bool loc)
                , ModelReadableLocation loc )
             => Value Bool loc
             -> HWT (ReadModel Bool loc, ReadModel Bool loc)
toggleModels bv = do
  idModel <- readModel bv
  notModel <- negateModel idModel
  return (idModel,notModel)

panel' es = panel es Nothing Nothing 

buttonC :: String -> HWTAction () -> HWT Element
buttonC t action = do
  m <- constModel t
  button m action

submitButtonC :: ( Data a
                 , Monoid a )
              => String
              -> TransientValue a
              -> (a -> HWTAction ())
              -> HWT Element
submitButtonC l tv action = do
  m <- constModel l
  submitButton m tv action

copyButtonC :: ( MonadHWT m
              , HWTPrefix (Value b loc1)
              , HWTPrefix (Value b loc2))
           => String
           -> Value b loc1
           -> Value b loc2
           -> m Element
copyButtonC t from to = do
  m <- constModel t
  copyButton m from to

labelCP :: (MonadHWT m) => String -> m Element
labelCP t = do
  l <- labelC t
  panel' [l]

setValueIn :: ( HWTPrefix (Value a loc)
              , HWTPrefix (Value b loc)
              , Data a
              , Data b
              , HWTActionAccessibleValueLocation loc
              , HWTActionSetableValueLocation loc
              , HWTInitAccessibleValue (Value a loc)
              , HWTInitAccessibleValue (Value b loc)
              )
           => Value a loc
           -> Projection a b
           -> b
           -> HWTAction ()
setValueIn v p x = do
  v' <- projection p v
  setValue v' x

negateModelV :: ( ModelReadableLocation loc
                , HWTPrefix (Value Bool loc) )
             => Value Bool loc -> HWT (ReadModel Bool loc)
negateModelV bv = do
  (_,nm) <- toggleModels bv
  return nm

textfieldT :: (MonadHWT m) => String -> m (Element,TransientValue String)
textfieldT t = do
  tv <- transientValue t
  m <- readWriteModel tv
  t <- textfield m
  return (t,tv)

data User = U { name :: String } deriving (Eq,Ord,Show,Typeable,Data)
data Room = R { roomname :: String
              , users :: [User]
              , chatlog :: [(User,String)]
              , msgCount :: Int } deriving (Show,Typeable,Data)
data ChatApp = C { rooms :: [(String,Room)] } deriving (Show,Typeable,Data)

$(derivePs ''ChatApp)
$(derivePs ''Room)
$(derivePs ''User)

newChatApp = C { rooms = [newRoom drn] }
newRoom n = (n, R { roomname = n
                  , users = []
                  , chatlog = []
                  , msgCount = 0})
newUser = U { name = "" }

drn = "default"

fil _ [] = []
fil p (x:xs) | p x = x : fil p xs
             | otherwise = xs

quitroom :: User -> String -> [(String,Room)] -> [(String,Room)]
quitroom u rn (room@(rn',r) : rs) | rn == rn' = (rn,r{users=fil (/= u) $ users r}) : rs
                                  | otherwise = room : quitroom u rn rs
quitroom _ _ [] = []

joinroom :: User -> String -> [(String,Room)] -> [(String,Room)]
joinroom u rn roomsmap = case lookup rn roomsmap of
  Nothing -> assoc rn R{ roomname=rn
                       , users=[u]
                       , chatlog=[]
                       , msgCount=0} roomsmap
  Just room -> assoc rn room{users= sort $ u : users room} roomsmap

switchroom :: User -> String -> String -> [(String,Room)] -> [(String,Room)]
switchroom u old new rm = joinroom u new $ quitroom u old rm

postmsg :: User -> String -> String -> [(String,Room)] -> [(String,Room)]
postmsg u msg rn rm = case lookup rn rm of
  Nothing -> postmsg u msg rn $ joinroom u rn rm
  Just r@R{chatlog=cl,msgCount=msgc} -> assoc rn r{ chatlog=take 20 $ (u,msg) : cl
                                                  , msgCount=msgc+1} rm

cleanChat :: HWT Element
cleanChat = do
  loginVisibleV <- clientValue True
  chatVisibleM <- negateModelV loginVisibleV
  chatAppV <- serverValue newChatApp
  currentRoomnameV <- windowValue drn
  nextRoomnameV <- windowValue drn
  currentRoomnameM <- readModel currentRoomnameV
  userV <- clientValue newUser
  usernameV <- projection name_p userV
  loginP <- loginPanel chatAppV userV loginVisibleV
  roomsV <- projection rooms_p chatAppV
  valueListener nextRoomnameV $ \newRoomname -> do
    oldRoomname <- getValue currentRoomnameV
    roomsmap <- getValue roomsV
    user <- getValue userV
    let
      roomsmap' = switchroom user oldRoomname newRoomname roomsmap
    setValue roomsV roomsmap'
    setValue currentRoomnameV newRoomname
  msgTV <- transientValue ""
  msgDrainV <- windowValue ""
  valueListener msgDrainV $ \msg -> do
    user <- getValue userV
    roomname <- getValue currentRoomnameV
    modifyValue roomsV (postmsg user msg roomname)
    setValue msgTV ""

  chatList <- list roomsV $ \roomV' -> do
    roomV <- projection snd_p roomV'
    roomPanel roomV userV msgTV currentRoomnameM msgDrainV
  roomsP <- roomsPanel roomsV nextRoomnameV
  chatP <- panel [chatList,roomsP] (Just chatVisibleM) Nothing
  panel [loginP,chatP] Nothing Nothing

loginPanel :: ServerValue ChatApp -> ClientValue User -> ClientValue Bool -> HWT Element
loginPanel appV userCV visibleCV = do
  (usernameT,usernameTV) <- textfieldT ""
  joinChatB <- submitButtonC "<- that's my name" usernameTV $ \username -> when (username /= "") $ do
    setValueIn userCV name_p username
    setValue usernameTV ""
    setValue visibleCV False
    user <- getValue userCV
    roomsV <- projection rooms_p appV
    modifyValue roomsV (joinroom user drn)
  visibleM <- readModel visibleCV
  panel [usernameT,joinChatB] (Just visibleM) Nothing

roomPanel :: (MonadHWT m, ProjectionMonadHWT m) => ServerValue Room -> ClientValue User -> TransientValue String -> ReadModel String loc1 -> WindowValue String -> m Element
roomPanel roomV userCV msgTV currentRoomM msgDrainV = do
  thisRoomDescriptionL <- labelC "Current room: "
  roomnameV <- projection roomname_p roomV
  thisRoomNameM <- readModel roomnameV
  thisRoomNameL <- label thisRoomNameM
  chatlogV <- projection chatlog_p roomV
  usersV <- projection users_p roomV
  chatlogList <- list chatlogV $ \msgUsrV -> do
    msgV <- projection snd_p msgUsrV
    usrNameV <- projection (fst_p >=> name_p) msgUsrV
    msgM <- readModel msgV
    msgL <- label msgM
    unM <- readModel usrNameV
    unL <- label unM
    space <- labelC ": "
    panel' [unL,space,msgL]
  msgP <- messagePanel msgTV msgDrainV
  userList <- list usersV $ \userV -> do
    usernameV <- projection name_p userV
    userM <- readModel usernameV
    ul <- label userM
    panel' [ul]
  roomUsersL <- labelCP "Users in this room:"
  visibleM <- eqModel currentRoomM thisRoomNameM
  panel [ thisRoomDescriptionL
        , thisRoomNameL
        , chatlogList
        , msgP
        , roomUsersL 
        , userList
        ] (Just visibleM) Nothing

roomsPanel :: ServerValue [(String,Room)] -> WindowValue String -> HWT Element
roomsPanel roomsV nextRoomnameV = do
  roomsList <- list roomsV $ \roomV -> do
    thisRoomnameV <- projection fst_p roomV
    thisRoomnameM <- readModel thisRoomnameV
    thisRoomnameL <- label thisRoomnameM
    joinB <- copyButtonC "join" thisRoomnameV nextRoomnameV
    panel' [thisRoomnameL,joinB]
  (newRoomnameT,newRoomnameTV) <- textfieldT ""
  joinB <- copyButtonC "create new room" newRoomnameTV nextRoomnameV
  newRoomP <- panel' [newRoomnameT,joinB]
  allRoomsL <- labelCP "All rooms:"
  panel' [allRoomsL,roomsList,newRoomP]

messagePanel :: MonadHWT m => TransientValue String -> WindowValue String -> m Element
messagePanel msgTV msgDrainV = do
  msgM <- readWriteModel msgTV
  msgT <- textfield msgM
  sendB <- copyButtonC "send" msgTV msgDrainV
  panel' [msgT,sendB]

main = test cleanChat
