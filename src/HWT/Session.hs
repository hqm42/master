{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             FunctionalDependencies,
             OverlappingInstances,
             FlexibleInstances,
             UndecidableInstances #-}

module HWT.Session where

import HWT.Types
import qualified Data.GenericDiffMap as GDM

import Text.JSON
import Text.JSON.Generic
import Data.Dynamic
import Data.Maybe
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Concurrent.AdvSTM
import Control.Concurrent.AdvSTM.TVar
import Control.Concurrent.AdvSTM.TMVar
import Control.Monad.Reader
import Control.Monad.Writer

handler :: ( Data a
           , Data b
           , HWTPrefix (Value a loc)
           )
        => (Value a loc)
        -> (JSValue -> SessionUpdates)
        -> GDM.GDMapValue b
        -> HWTAction ()
handler v singleton gdv = do
  let
    json = toJSON2 v $ gdmv2gdmvJS gdv
  tell $ singleton json 

updateHandler :: ( Data a
                 , Data b
                 , HWTPrefix (Value a loc)
                 , HWTActionAccessibleValueLocation loc)
              => Value a loc
              -> ( GDM.Ref b
                -> b
                -> b
                -> GDM.GDMapValue b
                -> HWTAction () )
updateHandler v i _ _ gdv =
  handler v (\js -> (singleUpdate v) mempty{changedValues=M.singleton (ref2GValue i) js}) gdv

insertHandler :: ( Data a
                 , Data b
                 , HWTPrefix (Value a loc)
                 , HWTActionAccessibleValueLocation loc)
              => Value a loc
              -> (GDM.Ref b -> b -> GDM.GDMapValue b -> HWTAction ())
insertHandler v i _ gdv =
  handler v (\js -> (singleUpdate v) mempty{newValues=M.singleton (ref2GValue i) js}) gdv

deleteHandler :: ( Data a
                 , HWTPrefix (Value a loc)
                 , HWTActionAccessibleValueLocation loc)
              => Value a loc
              -> (GDM.GRef
              -> HWTAction () )
deleteHandler v i =
  handler v (\_ -> (singleUpdate v) mempty{removedValues=[HWTId i Nothing]}) dummyValue
  where
    dummyValue = GDM.GDPrimitive ()

update :: (HWTAction (TVar SessionMap)) -> (SessionMap -> HWTAction SessionMap) -> HWTAction ()
update get upd = do
  svsT <- get
  svs <- readTVar svsT
  svs' <- upd svs
  writeTVar svsT svs'

instance HWTActionAccessibleValueLocation ServerLocation where
  getSessionMap _ = asks windowServerValues
  singleUpdate _ = \ums -> mempty{serverValueUpdates=ums}
  callListeners (HWTId i _) as = do
    svsT <- asks windowServerValues
    svs <- readTVar svsT
    let
      jsM = GDM.serialize i svs
      lsM = IM.lookup i $ serverValueListeners as
    case lsM of
      Nothing -> return ()
      Just ls -> case jsM of
        Nothing -> return ()
        Just js -> mapM_ (\l -> HWT.Types.listen l js) ls


instance HWTActionAccessibleValueLocation ClientLocation where
  getSessionMap _ = asks windowClientValues
  singleUpdate _ = \ums -> mempty{clientValueUpdates=ums}
  callListeners (HWTId i _) as = do
    cvsT <- asks windowClientValues
    cvs <- readTVar cvsT
    let
      jsM = GDM.serialize i cvs
      lsM = IM.lookup i $ clientValueListeners as
    case lsM of
      Nothing -> return ()
      Just ls -> case jsM of
        Nothing -> return ()
        Just js -> mapM_ (\l -> HWT.Types.listen l js) ls

instance HWTActionAccessibleValueLocation WindowLocation where
  getSessionMap _ = asks windowWindowValues
  singleUpdate _ = \ums -> mempty{windowValueUpdates=ums}
  callListeners (HWTId i _) as = do
    wvsT <- asks windowWindowValues
    wvs <- readTVar wvsT
    let
      jsM = GDM.serialize i wvs
      lsM = IM.lookup i $ windowValueListeners as
    case lsM of
      Nothing -> return ()
      Just ls -> case jsM of
        Nothing -> return ()
        Just js -> mapM_ (\l -> HWT.Types.listen l js) ls

instance HWTActionAccessibleValueLocation loc => HWTActionGetableValueLocation loc where
  getValue v = do
    smT <- getSessionMap v
    sm <- readTVar smT
    case GDM.lookup (value2Ref v) sm of
      Nothing -> error $ "getValue: notFound " ++ show sm
      Just x -> return x

instance TransientPred ServerLocation HTrue
instance TransientPred ClientLocation HTrue
instance TransientPred WindowLocation HTrue

instance HWTActionAccessibleValueLocation loc => HWTActionSetableValueLocation' HTrue loc where
  setValue' _ v x = withSessionMap v $ \smT -> do
    sm <- readTVar smT
    sm' <- GDM.handleUpdate (value2Ref v) x (updateHandler v) (insertHandler v) (deleteHandler v) sm
    writeTVar smT sm'
  deserializeValue' _ v@(HWTId i _) js = withSessionMap v $ \smT -> do
    sm <- readTVar smT
    sm' <- GDM.handleDeserialize i js (updateHandler v) (insertHandler v) (deleteHandler v) sm
    writeTVar smT sm'

modifyValue :: ( Data a
               , HWTPrefix (Value a loc)
               , HWTActionGetableValueLocation loc
               , HWTActionSetableValueLocation loc)
            => (Value a loc)
            -> (a -> a)
            -> HWTAction ()
modifyValue v f = do
  vc <- getValue v
  setValue v $ f vc

instance HWTActionSetableValueLocation TransientLocation where
  setValue v x = do
    tell mempty{transientValueUpdates=mempty{changedValues=M.singleton (ref2GValue $ value2Ref v) (toJSON2 v $ GDM.GDPrimitive $ toJSON x)}}
  deserializeValue = undefined
