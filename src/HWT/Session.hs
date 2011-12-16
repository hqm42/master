{-# LANGUAGE FlexibleContexts #-}
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


type HWTAction = WriterT SessionUpdates (ReaderT HWTSession AdvSTM)

getServerValues :: HWTAction SessionMap
getServerValues = do
  svsT <- asks serverValues
  readTVar svsT

setServerValues :: SessionMap -> HWTAction ()
setServerValues svs' = do
  svsT <- asks serverValues
  writeTVar svsT svs'

handler :: Data a => (JSValue -> SessionUpdates) -> GDM.GDMapValue a -> HWTAction ()
handler singleton gdv = do
  let
    json = gdv2JSON gdv
  tell $ singleton json 

updateHandler :: Data a => (UpdateMaps (Value () at) -> SessionUpdates) -> GDM.Ref a -> a -> a -> GDM.GDMapValue a -> HWTAction ()
updateHandler set i _ _ gdv =
  handler (\js -> set mempty{changedValues=M.singleton (ref2GValue i) js}) gdv

insertHandler :: forall a loc. Data a => (UpdateMaps (Value () loc) -> SessionUpdates) -> (GDM.Ref a -> a -> GDM.GDMapValue a -> HWTAction ())
insertHandler set i _ gdv =
  handler (\js -> set mempty{newValues=M.singleton (ref2GValue i) js}) gdv

deleteHandler :: (UpdateMaps (Value () at) -> SessionUpdates) -> GDM.GRef -> HWTAction ()
deleteHandler set i =
  handler (\_ -> set mempty{removedValues=[HWTId i]}) dummyValue
  where
    dummyValue = GDM.GDPrimitive ()

setValue :: Data a
         => ((SessionMap -> HWTAction SessionMap) -> HWTAction ())
         -> (UpdateMaps (Value () loc) -> SessionUpdates)
         -> Value a loc
         -> a
         -> HWTAction ()
setValue updateValues singleton i x = do
  updateValues $ GDM.handleUpdate (value2Ref i) x (updateHandler singleton) (insertHandler singleton) (deleteHandler singleton)

update :: (HWTAction (TVar SessionMap)) -> (SessionMap -> HWTAction SessionMap) -> HWTAction ()
update get upd = do
  svsT <- get
  svs <- readTVar svsT
  svs' <- upd svs
  writeTVar svsT svs'

setServerValue :: Data a => ServerValue a -> a -> HWTAction ()
setServerValue = setValue (update (asks serverValues)) (\um -> mempty{serverValueUpdates=um})

setClientValue :: Data a => ClientValue a -> a -> HWTAction ()
setClientValue = setValue (update (asks clientValues)) (\um -> mempty{clientValueUpdates=um})

-- all transient values are handled as primitives. no partial update is possible
setTransientValue :: Data a => TransientValue a -> a -> HWTAction ()
setTransientValue i x = updateHandler 
                          (\um -> mempty{transientValueUpdates=um})
                          (value2Ref i)
                          undefined -- unused FIXME
                          undefined -- unused FIXME
                          (GDM.GDPrimitive x)

getServerValue :: Data a => ServerValue a -> HWTAction a
getServerValue i = do
  svs <- getServerValues
  return $ fromJust $ GDM.lookup (value2Ref i) svs

getClientValue :: Data a => ClientValue a -> HWTAction a
getClientValue = undefined

-- TESTS

sessionTest1 :: IO ()
sessionTest1 = do
  let
    (svRef,globalValues) = GDM.insert [["Hallo","Welt"],["foo","bar"]] GDM.newJSONGDMap
  globalValuesT <- newTVarIO (globalValues :: SessionMap)
  let
    session = HWTSession undefined globalValuesT undefined undefined
  x <- atomically (runReaderT (runWriterT (setServerValue (HWTId 0 :: ServerValue [[String]]) [["Hallo"],["foo","bar","Welt"],["42"]])) session)
  putStrLn $ show $ serverValueUpdates $ snd $ x
  return ()



