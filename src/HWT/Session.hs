{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module HWT.Session where

import HWT.Types
import qualified Data.GenericDiffMap as GDM

import Text.JSON
import Text.JSON.Generic
import Data.Dynamic
import Data.Maybe
import qualified Data.Map as M
import Control.Concurrent.AdvSTM
import Control.Concurrent.AdvSTM.TVar
import Control.Concurrent.AdvSTM.TMVar
import Control.Monad.Reader
import Control.Monad.Writer

handler :: Data a => (JSValue -> SessionUpdates) -> GDM.GDMapValue a -> HWTAction ()
handler singleton gdv = do
  let
    json = gdv2JSON gdv
  tell $ singleton json 

updateHandler :: (HWTActionAccessibleValueLocation loc, Data b)
              => Value a loc
              -> ( GDM.Ref b
                -> b
                -> b
                -> GDM.GDMapValue b
                -> HWTAction () )
updateHandler v i _ _ gdv =
  handler (\js -> (singleUpdate v) mempty{changedValues=M.singleton (ref2GValue i) js}) gdv

insertHandler :: (HWTActionAccessibleValueLocation loc, Data b)
              => Value a loc
              -> (GDM.Ref b -> b -> GDM.GDMapValue b -> HWTAction ())
insertHandler v i _ gdv =
  handler (\js -> (singleUpdate v) mempty{newValues=M.singleton (ref2GValue i) js}) gdv

deleteHandler :: (HWTActionAccessibleValueLocation loc)
              => Value a loc
              -> (GDM.GRef -> HWTAction () )
deleteHandler v i =
  handler (\_ -> (singleUpdate v) mempty{removedValues=[HWTId i Nothing]}) dummyValue
  where
    dummyValue = GDM.GDPrimitive ()

update :: (HWTAction (TVar SessionMap)) -> (SessionMap -> HWTAction SessionMap) -> HWTAction ()
update get upd = do
  svsT <- get
  svs <- readTVar svsT
  svs' <- upd svs
  writeTVar svsT svs'

instance HWTActionAccessibleValueLocation ServerLocation where
  getSessionMap _ = asks serverValues
  singleUpdate _ = \ums -> mempty{serverValueUpdates=ums}

instance HWTActionAccessibleValueLocation ClientLocation where
  getSessionMap _ = asks clientValues
  singleUpdate _ = \ums -> mempty{clientValueUpdates=ums}

instance HWTActionAccessibleValueLocation loc => HWTActionGetableValueLocation loc where
  getValue v = do
    smT <- getSessionMap v
    sm <- readTVar smT
    case GDM.lookup (value2Ref v) sm of
      Nothing -> error $ "getValue: notFound " ++ show sm
      Just x -> return x

instance HWTActionAccessibleValueLocation loc => HWTActionSetableValueLocation loc where
  setValue v x = withSessionMap v $ \smT -> do
    sm <- readTVar smT
    sm' <- GDM.handleUpdate (value2Ref v) x (updateHandler v) (insertHandler v) (deleteHandler v) sm
    writeTVar smT sm'

instance HWTActionSetableValueLocation TransientLocation where
  setValue v x = do
    tell mempty{transientValueUpdates=mempty{changedValues=M.singleton (ref2GValue $ value2Ref v) (gdv2JSON $ GDM.GDPrimitive x)}}

-- TESTS
