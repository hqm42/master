{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HWT.Types where

import qualified Data.GenericDiffMap as GDM

import Text.JSON
import Text.JSON.Generic
import Data.Data
import Data.Maybe
import Data.Monoid
import Data.Dynamic
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Concurrent.AdvSTM.TVar
import Control.Concurrent.AdvSTM.TMVar
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Concurrent.AdvSTM
import Control.Concurrent.AdvSTM.TVar
import Yesod.Static

data HWTId a b = HWTId Int (Maybe b) deriving (Eq,Ord)

newtype SessionKey = SK String deriving (Eq,Ord,Show)
newtype WindowKey = WK String deriving (Eq,Ord,Show)

-- monads
type HWT = State HWTInit
type HWTAction = WriterT SessionUpdates (ReaderT HWTWindow AdvSTM)
type HWTPageReload = Reader HWTPageReloadContext

-- conversions
value2Ref :: Value a at -> GDM.Ref a
value2Ref (HWTId i _) = GDM.Ref i

ref2GValue :: GDM.Ref a -> Value () at
ref2GValue i = HWTId (GDM.gRef i) Nothing

ref2Value :: HWTPrefix (Value a at) => GDM.Ref a -> Value a at
ref2Value i = HWTId (GDM.gRef i) Nothing

gdv2JSON :: Data a => GDM.GDMapValue a -> JSValue
gdv2JSON gdv = case gdv of
           GDM.GDPrimitive x' -> toJSON x'
           c -> complex2JSON c

complex2JSON :: GDM.GDMapValue a -> JSValue
complex2JSON (GDM.GDComplex cn cs) = JSObject $ toJSObject [ ("constructorName",toJSON cn)
                                                           , ("childRefs",toJSON cs)]


-- Types
type Value a location = HWTId (ValueTag,a,location) ()
type ServerValue a = Value a ServerLocation
type ClientValue a = Value a ClientLocation
type WindowValue a = Value a WindowLocation
type TransientValue a = Value a TransientLocation

type Model a accesstype loc = HWTId (ModelTag,a,accesstype) (Value a loc)
type ReadModel a loc = Model a ReadAccess loc
type WriteModel a loc = Model a WriteAccess loc
type ReadWriteModel a loc = Model a ReadWriteAccess loc

type Element = HWTId ElementTag ()

type Listener = HWTId ListenerTag ()

-- Tags
data ValueTag
data ModelTag
data ElementTag
data ListenerTag

-- Locations
data ServerLocation
data ClientLocation
data TransientLocation
data WindowLocation

class ModelReadableLocation loc
instance ModelReadableLocation ServerLocation
instance ModelReadableLocation ClientLocation
instance ModelReadableLocation WindowLocation
instance ModelReadableLocation TransientLocation

class ModelWriteableLocation loc
instance ModelWriteableLocation ClientLocation
instance ModelWriteableLocation WindowLocation
instance ModelWriteableLocation TransientLocation

class ModelReadableWriteableLocation loc
instance (ModelReadableLocation loc, ModelWriteableLocation loc) => ModelReadableWriteableLocation loc

-- Accesstypes
data ReadAccess
data WriteAccess
data ReadWriteAccess

-- Show/Read instances

class HWTParseId a where
  toHWTId :: String -> Maybe a
  fromHWTId :: a -> String

prefix p (HWTId i _) = p ++ show i

dropPrefix p s = if p == take pl s
  then Just $ HWTId (read $ drop pl s) Nothing
  else Nothing
  where
    pl = length p

instance HWTParseId (HWTId a b) => Show (HWTId a b) where
  show = fromHWTId

instance HWTParseId (HWTId a b) => Read (HWTId a b) where
  readsPrec _ s = case toHWTId s of
    Just x -> [(x,"")]
    Nothing -> []

-- getPrefix must not evaluate the argument
class HWTPrefix a where
  getPrefix :: a -> String

instance HWTPrefix (HWTId a b) => HWTParseId (HWTId a b) where
  toHWTId s = x
    where
      x = dropPrefix (getPrefix $ fromJust x) s
  fromHWTId x = prefix (getPrefix x) x

instance HWTPrefix (ServerValue a) where
  getPrefix _ = "vS"

instance HWTPrefix (ClientValue a) where
  getPrefix _ = "vC"

instance HWTPrefix (WindowValue a) where
  getPrefix _ = "vW"

instance HWTPrefix (TransientValue a) where
  getPrefix _ = "vT"

instance HWTPrefix (ReadModel a loc) where
  getPrefix _ = "mR"

instance HWTPrefix (WriteModel a loc) where
  getPrefix _ = "mW"

instance HWTPrefix (ReadWriteModel a loc) where
  getPrefix _ = "mRW"

instance HWTPrefix (Element) where
  getPrefix _ = "e"


instance HWTPrefix a => HWTPrefix (a,b) where
  getPrefix (x,_) = getPrefix x

-- Session Data

type SessionMap = GDM.GDMap Dynamic JSValue
data UpdateMaps a = UpdateMaps { newValues :: UpdateMap a
                               , changedValues :: UpdateMap a
                               , removedValues :: [a] } deriving Show

instance Ord a => Monoid (UpdateMaps a) where
  mempty = UpdateMaps mempty mempty mempty
  mappend (UpdateMaps n c r) (UpdateMaps n' c' r') = UpdateMaps (mappend n' n) (mappend c' c) (r ++ r')

type UpdateMap a = M.Map a JSValue

data SessionUpdates = SessionUpdates {
  serverValueUpdates :: UpdateMaps (ServerValue ()),
  clientValueUpdates :: UpdateMaps (ClientValue ()),
  windowValueUpdates :: UpdateMaps (WindowValue ()),
  transientValueUpdates :: UpdateMaps (TransientValue ())
} deriving Show

instance Monoid SessionUpdates where
  mempty = SessionUpdates mempty mempty mempty mempty
  mappend (SessionUpdates s c w t) (SessionUpdates s' c' w' t') = SessionUpdates (mappend s s') (mappend c c') (mappend w w') (mappend t t')

data HWTApplicationState = HWTApplicationState {
  applicationServerValues :: TVar SessionMap,
  applicationSessions :: TVar (M.Map SessionKey HWTSession),
  serverValueListeners :: ValueListeners,
  clientValueListeners :: ValueListeners,
  windowValueListeners :: ValueListeners,
  newSession :: IO HWTSession,
  applicationInit :: (Element,[ConstructorCall]),
  getStaticFiles :: Static
}

data HWTSession = HWTSession {
  sessionServerValues :: TVar SessionMap,
  sessionClientValues :: TVar SessionMap,
  sessionWindows :: TVar (M.Map WindowKey HWTWindow),
  newWindow :: IO HWTWindow
}

data HWTWindow = HWTWindow {
  windowServerValues :: TVar SessionMap,
  windowClientValues :: TVar SessionMap,
  windowWindowValues :: TVar SessionMap,
  windowUpdates :: TMVar SessionUpdates,
  windowSessions :: TVar (M.Map SessionKey HWTSession),
  windowWindows :: TVar (M.Map WindowKey HWTWindow)
}

data HWTPageReloadContext = HWTPageReloadContext {
  pageReloadServerValues :: SessionMap,
  pageReloadClinetValues :: SessionMap
}

-- Session/Global Variables
class HWTActionAccessibleValueLocation loc where
  getSessionMap :: Value a loc -> HWTAction (TVar SessionMap)
  singleUpdate :: Value a loc -> (UpdateMaps (Value () loc) -> SessionUpdates)
  callListeners :: Value () loc -> HWTApplicationState -> HWTAction ()

withSessionMap :: HWTActionAccessibleValueLocation loc => Value a loc -> (TVar SessionMap -> HWTAction b) -> HWTAction b
withSessionMap v f = do
  smT <- getSessionMap v
  f smT

class HWTActionGetableValueLocation loc where
  getValue :: Data a => Value a loc -> HWTAction a

class HWTActionSetableValueLocation loc where
  setValue :: Data a => Value a loc -> a -> HWTAction ()
  deserializeValue :: Value () loc -> JSValue -> HWTAction ()

-- Initialization

data HWTInit = HWTInit { nextId :: Int
                       , constructorCalls :: [ConstructorCall]
                       , initialServerValues :: SessionMap
                       , initialClientValues :: SessionMap
                       , initialWindowValues :: SessionMap
                       , initialServerValueListeners :: ValueListeners
                       , initialClientValueListeners :: ValueListeners
                       , initialWindowValueListeners :: ValueListeners} deriving Show

data ConstructorCall = ConstructorCall { referenceName :: String
                                       , constructorCall :: HWTPageReload String }

instance Show ConstructorCall where
  show (ConstructorCall ref impl) = "var " ++ ref ++ " = XXX;"

class HWTInitAccessibleValue a where
  getIntId :: a -> Int
  getInitSessionMap :: a -> HWT SessionMap
  getInitListeners :: a -> HWT ValueListeners
  setInitListeners :: a -> ValueListeners -> HWT ()

withInitSessionMap :: HWTInitAccessibleValue a => a -> (SessionMap -> HWT b) -> HWT b
withInitSessionMap v f = do
  sm <- getInitSessionMap v
  f sm

addListener :: HWTInitAccessibleValue v => v -> ValueListener -> HWT ()
addListener v l = do
  ls <- getInitListeners v
  let
    i = getIntId v
    ls' = IM.unionWith mappend ls $ IM.singleton i [l]
  setInitListeners v ls'

instance HWTInitAccessibleValue (ServerValue a) where
  getIntId (HWTId i _) = i
  getInitSessionMap _ = gets initialServerValues
  getInitListeners _ = gets initialServerValueListeners
  setInitListeners _ ls = modify $ \ini -> ini{initialServerValueListeners=ls}

instance HWTInitAccessibleValue (ClientValue a) where
  getIntId (HWTId i _) = i
  getInitSessionMap _ = gets initialClientValues
  getInitListeners _ = gets initialClientValueListeners
  setInitListeners _ ls = modify $ \ini -> ini{initialClientValueListeners=ls}

instance HWTInitAccessibleValue (WindowValue a) where
  getIntId (HWTId i _) = i
  getInitSessionMap _ = gets initialWindowValues
  getInitListeners _ = gets initialWindowValueListeners
  setInitListeners _ ls = modify $ \ini -> ini{initialWindowValueListeners=ls}

data ValueListener = ValueListener{
  listen :: HWTAction ()
}

instance Show ValueListener where
  show _ = "ValueListener"

type ValueListeners = IM.IntMap [ValueListener]
