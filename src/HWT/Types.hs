{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HWT.Types where

import qualified Data.GenericDiffMap as GDM

import Text.JSON
import Text.JSON.Generic
import Data.Data
import Data.Maybe
import Data.Monoid
import Data.Dynamic
import qualified Data.Map as M
import Control.Concurrent.AdvSTM.TVar
import Control.Concurrent.AdvSTM.TMVar

data HWTId a = HWTId Int deriving (Eq,Ord)

value2Ref :: Value a at -> GDM.Ref a
value2Ref (HWTId i) = GDM.Ref i

ref2GValue :: GDM.Ref a -> Value () at
ref2GValue i = HWTId $ GDM.gRef i

ref2Value :: GDM.Ref a -> Value a at
ref2Value i = HWTId $ GDM.gRef i

gdv2JSON :: Data a => GDM.GDMapValue a -> JSValue
gdv2JSON gdv = case gdv of
           GDM.GDPrimitive x' -> toJSON x'
           GDM.GDComplex cn cs -> JSObject $ toJSObject [ ("constructorName",toJSON cn)
                                                    , ("childRefs",toJSON cs)]

-- Types
type Value a location = HWTId (ValueTag,a,location)
type ServerValue a = Value a ServerLocation
type ClientValue a = Value a ClientLocation
type TransientValue a = Value a TransientLocation

type Model a accesstype = HWTId (ModelTag,a,accesstype)
type ReadModel a = Model a ReadAccess
type WriteModel a = Model a WriteAccess
type ReadWriteModel a = Model a ReadWriteAccess

type Element = HWTId ElementTag

-- Tags
data ValueTag
data ModelTag
data ElementTag

-- Locations
data ServerLocation
data ClientLocation
data TransientLocation

-- Accesstypes
data ReadAccess
data WriteAccess
data ReadWriteAccess

-- Show/Read instances

class HWTParseId a where
  toHWTId :: String -> Maybe a
  fromHWTId :: a -> String

prefix p (HWTId i) = p ++ show i
dropPrefix p s = if p == take pl s
  then Just $ HWTId $ read $ drop pl s
  else Nothing
  where
    pl = length p

instance HWTParseId (HWTId a) => Show (HWTId a) where
  show = fromHWTId

instance HWTParseId (HWTId a) => Read (HWTId a) where
  readsPrec _ s = case toHWTId s of
    Just x -> [(x,"")]
    Nothing -> []

-- getPrefix must not evaluate the argument
class HWTPrefix a where
  getPrefix :: a -> String

instance HWTPrefix (HWTId a) => HWTParseId (HWTId a) where
  toHWTId s = x
    where
      x = dropPrefix (getPrefix $ fromJust x) s
  fromHWTId x = prefix (getPrefix x) x

instance HWTPrefix (ServerValue a) where
  getPrefix _ = "vS"

instance HWTPrefix (ClientValue a) where
  getPrefix _ = "vC"

instance HWTPrefix (TransientValue a) where
  getPrefix _ = "vT"

instance HWTPrefix (ReadModel a) where
  getPrefix _ = "mR"

instance HWTPrefix (WriteModel a) where
  getPrefix _ = "mW"

instance HWTPrefix (ReadWriteModel a) where
  getPrefix _ = "mRW"

instance HWTPrefix (Element) where
  getPrefix _ = "e"

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
  transientValueUpdates :: UpdateMaps (TransientValue ())
} deriving Show

instance Monoid SessionUpdates where
  mempty = SessionUpdates mempty mempty mempty
  mappend (SessionUpdates s c t) (SessionUpdates s' c' t') = SessionUpdates (mappend s s') (mappend c c') (mappend t t')

data HWTSession = HWTSession {
  clientValues :: TVar SessionMap,
  serverValues :: TVar SessionMap,
  updates :: TMVar SessionUpdates,
  allSessions :: TVar [TVar HWTSession]
}

data ApplicationState = ApplicationState {
  globalValues :: TVar SessionMap,
  sessions :: TVar [HWTSession]
}
