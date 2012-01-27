{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HWT.Tests where

import Control.Monad (when,(>=>))
import Data.Data
import List (sort)

import Data.GenericDiffMap.Projection
import HWT.Types
import HWT.Core
import HWT.Session
import HWT.Server

-- TESTS

test0 :: IO ()
test0 = runHWTApp 8080 $ do
  v1 <- windowValue ("Hallo Welt" :: String)
  v2 <- clientValue ("" :: String)

  valueListener v1 $ \v1content ->  do
    setValue v2 $ reverse v1content

  mRW <- readWriteModel v1
  t <- textfield' mRW

  l <- label v2

  panel [t,l] True ("" :: String) 

test1 :: IO ()
test1 = runHWTApp 8080 $ do
  windowClicks <- windowValue (0 :: Integer)
  sessionClicks <- clientValue (0 :: Integer)
  serverClicks <- serverValue (0 :: Integer)
  m2 <- readModel windowClicks
  m3 <- readModel sessionClicks
  m4 <- readModel serverClicks
  b1 <- button "Hallo Welt" (do
    modifyValue windowClicks (+1)
    modifyValue sessionClicks (+1)
    modifyValue serverClicks (+1)) False ""
  wl <- label ("windowclicks:" :: String)
  l1 <- label m2

  cl <- label ("clientclicks:" :: String)
  l2 <- label m3

  sl <- label ("serverclicks" :: String)
  l3 <- label m4
  panel [b1,wl,l1,cl,l2,sl,l3] True ("" :: String) 

test2 :: IO ()
test2 = runHWTApp 8080 $ do
  lv <- serverValue [1,2,3]
  next <- serverValue (4 :: Int)
  bl1 <- constModel ("WENIGER!" :: String)
  bl2 <- constModel ("MEHR!" :: String)
  b1 <- button bl1 (do
    modifyValue lv $ \nn -> case nn of
      [] -> []
      (_:t) -> t) False ""
  b2 <- button bl2 (do
    n <- getValue next
    modifyValue lv (++[n])
    setValue next $ n+1) False ""
  l <- list' lv $ \v -> do
    m <- readModel v
    l1 <- label m
    l2 <- label (" == " :: String)
    l3 <- label m
    panel [l1,l2,l3] True ("" :: String)
  panel [b1,b2,l] True ("" :: String)

test3 :: IO ()
test3 = runHWTApp 8080 $ do
  vt <- transientValue ("" :: String)
  mt <- readWriteModel vt
  vs <- serverValue ("" :: String)
  ms <- readModel vs
  t <- textfield' mt
  l <- label ms
  bl <- constModel ("->" :: String)
  b <- submitButton bl vt (\ text -> do
    setValue vs text) False ""
  panel [t,b,l] True ("" :: String)

-- huge chat example

assoc :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
assoc key val [] = [(key,val)]
assoc key val (p@(key',_):xs) | key == key' = (key,val):xs
                              | otherwise = p:(assoc key val xs)

data User = U { name :: String } deriving (Eq,Ord,Show,Typeable,Data)
data Room = R { roomname :: String
              , users :: [User]
              , chatlog :: [(User,String)]
              , msgCount :: Int } deriving (Show,Typeable,Data)
data ChatApp = C { rooms :: [(String,Room)] } deriving (Show,Typeable,Data)

$(derivePs ''ChatApp)
$(derivePs ''Room)
$(derivePs ''User)

drn = "default"
newChatApp = C { rooms = [newRoom drn] }
newRoom n = (n, R { roomname = n
                  , users = []
                  , chatlog = []
                  , msgCount = 0})
newUser = U { name = "" }

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

chat :: HWT Element
chat = do
  addCSS "foo.css"
  loginVisibleV <- clientValue True
  chatVisibleM <- negateModel loginVisibleV
  chatAppV <- serverValue newChatApp
  currentRoomnameV <- windowValue drn
  nextRoomnameV <- windowValue drn
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
  chatList <- list' roomsV $ \roomV' -> do
    roomV <- projection snd_p roomV'
    roomPanel roomV userV msgTV currentRoomnameV msgDrainV
  roomsP <- roomsPanel roomsV nextRoomnameV
  chatP <- panel [chatList,roomsP] chatVisibleM "chatPanel"
  panel [loginP,chatP] True "root"

loginPanel :: ServerValue ChatApp -> ClientValue User -> ClientValue Bool -> HWT Element
loginPanel appV userCV visibleCV = do
  (usernameT,usernameTV) <- textfieldT' ""
  joinChatB <- submitButton "<- that's my name" usernameTV (\username -> when (username /= "") $ do
    setValueIn userCV name_p username
    setValue usernameTV ""
    setValue visibleCV False
    user <- getValue userCV
    roomsV <- projection rooms_p appV
    modifyValue roomsV (joinroom user drn)) False ""
  panel [usernameT,joinChatB] visibleCV "loginPanel"

roomPanel :: (MonadHWT m, ProjectionMonadHWT m) => ServerValue Room -> ClientValue User -> TransientValue String -> WindowValue String -> WindowValue String -> m Element
roomPanel roomV userCV msgTV currentRoomV msgDrainV = do
  thisRoomDescriptionL <- label "Current room: "
  roomnameV <- projection roomname_p roomV
  thisRoomNameL <- label roomnameV
  chatlogV <- projection chatlog_p roomV
  usersV <- projection users_p roomV
  chatlogList <- list' chatlogV $ \msgUsrV -> do
    msgV <- projection snd_p msgUsrV
    usrNameV <- projection (fst_p >=> name_p) msgUsrV
    msgL <- label msgV
    unL <- label usrNameV
    space <- label ": "
    panel [unL,space,msgL] True "msg"
  msgP <- messagePanel msgTV msgDrainV
  userList <- list' usersV $ \userV -> do
    usernameV <- projection name_p userV
    ul <- label usernameV
    panel [ul] True "user"
  roomUsersL' <- label "Users in this room:"
  roomUsersL <- panel' [roomUsersL']
  visibleM <- eqModel currentRoomV roomnameV
  panel [ thisRoomDescriptionL
        , thisRoomNameL
        , chatlogList
        , msgP
        , roomUsersL 
        , userList
        ] visibleM "room"

roomsPanel :: ServerValue [(String,Room)] -> WindowValue String -> HWT Element
roomsPanel roomsV nextRoomnameV = do
  roomsList <- list' roomsV $ \roomV -> do
    thisRoomnameV <- projection fst_p roomV
    thisRoomnameL <- label thisRoomnameV
    joinB <- copyButton "join" thisRoomnameV nextRoomnameV False ""
    msgcL <- projection (snd_p >=> msgCount_p) roomV >>= label
    panel [thisRoomnameL,joinB,msgcL] True "joinRoom"
  (newRoomnameT,newRoomnameTV) <- textfieldT' ""
  joinB <- copyButton "create new room" newRoomnameTV nextRoomnameV False ""
  newRoomP <- panel [newRoomnameT,joinB] True "newRoom"
  allRoomsL' <- label "All rooms:"
  allRoomsL <- panel' [allRoomsL']
  panel [allRoomsL,roomsList,newRoomP] True "roomsPanel"

messagePanel :: MonadHWT m => TransientValue String -> WindowValue String -> m Element
messagePanel msgTV msgDrainV = do
  msgM <- readWriteModel msgTV
  msgT <- textfield' msgM
  sendB <- copyButton "send" msgTV msgDrainV False ""
  panel' [msgT,sendB]

main = runHWTApp 8080 chat
