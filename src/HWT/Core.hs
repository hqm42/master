{-# LANGUAGE FlexibleContexts #-}
module HWT.Core where

import HWT.Types
import qualified Data.GenericDiffMap as GDM

import Control.Monad.State
import Data.Data
import Text.JSON.Generic
import Text.JSON.Pretty
import qualified Data.IntMap as IM




newHWTInit :: HWTInit
newHWTInit = HWTInit 0 [] GDM.newJSONGDMap GDM.newJSONGDMap IM.empty IM.empty

withNewHWTId :: Maybe b -> (HWTId a b -> HWT ()) -> HWT (HWTId a b)
withNewHWTId meta a = do
  i <- newHWTId meta
  a i
  return i

renderJSON :: JSValue -> String
renderJSON = render . pp_value

newHWTId :: Maybe b -> HWT (HWTId a b)
newHWTId meta = do
  i <- gets nextId
  modify $ \ini -> ini{nextId = i + 1}
  let
    res = HWTId i meta
  return $ res

addConstructorCall :: String -> String -> HWT ()
addConstructorCall refName impl = modify $ \ini@HWTInit{constructorCalls=cc} -> ini{constructorCalls=cc++[cv]}
  where
    cv = ConstructorCall refName impl

clientValue :: Data a => a -> HWT (ClientValue a)
clientValue x = do
  icvs <- gets initialClientValues
  (ref,icvs') <- GDM.handleInsert x (\r _ gdmv -> clientValue' (ref2Value r) gdmv) icvs
  modify $ \ini -> ini{initialClientValues=icvs'}
  return (ref2Value ref)

clientValue' :: Data a => ClientValue a -> GDM.GDMapValue a -> HWT ()
clientValue' i gdv = addConstructorCall refName impl
  where
    refName = show i
    js = renderJSON $ gdv2JSON gdv
    impl = "new ClientValue('" ++ refName ++ "'," ++ js ++ ")"

serverValue :: Data a => a -> HWT (ServerValue a)
serverValue x = do
  icvs <- gets initialServerValues
  (ref,isvs') <- GDM.handleInsert x (\r _ _ -> serverValue' (ref2Value r)) icvs
  modify $ \ini -> ini{initialServerValues=isvs'}
  return (ref2Value ref)

serverValue' :: Data a => ServerValue a -> HWT ()
serverValue' i = addConstructorCall refName impl
  where
    refName = show i
    impl = "new ServerValue('" ++ refName ++ "')"

transientValue :: Data a => a -> HWT (TransientValue a)
transientValue x = withNewHWTId Nothing $ \i -> do
  let
    refName = show i
    js = renderJSON $ toJSON x
    impl = "new TransientValue('" ++ refName ++ "'," ++ js ++ ")"
  addConstructorCall refName impl

model' :: String -> String -> String -> HWT ()
model' refName m v = do
  let
    impl = "new " ++ m ++ "(" ++ v ++ ")"
  addConstructorCall refName impl
  return ()

readModel :: (ModelReadableLocation loc, HWTPrefix (Value a loc), Data a)
          => Value a loc
          -> HWT (ReadModel a loc)
readModel v = withNewHWTId (Just v) $ \i -> model' (show i) "ReadModel" (show v)

readWriteModel :: (ModelReadableWriteableLocation loc, HWTPrefix (Value a loc), Data a)
               => Value a loc
               -> HWT (ReadWriteModel a loc)
readWriteModel v = withNewHWTId (Just v) $ \i ->  model' (show i) "ReadWriteModel" (show v)

writeModel :: (ModelWriteableLocation loc, HWTPrefix (Value a loc), Data a)
           => Value a loc
           -> HWT (WriteModel a loc)
writeModel v = withNewHWTId (Just v) $ \i ->  model' (show i) "WriteModel" (show v)

newElement = withNewHWTId Nothing

label :: ReadModel a loc -> HWT Element
label m = newElement $ \i -> do
  let
    impl = "new Label(" ++ (show m) ++ ")"
  addConstructorCall (show i) impl

projection :: ( Data a
              , Data b
              , HWTInitAccessibleValue (Value a loc)
              , HWTPrefix (Model b at loc)
              , HWTPrefix (Value b loc))
           => GDM.Projection a b
           -> Model a at loc
           -> HWT (Model b at loc)
projection p (HWTId _ (Just v)) = do
  withInitSessionMap v $ \sm -> withNewHWTId (Just $ ref2Value $ GDM.projection p (value2Ref v) sm) $ const $ return ()

valueListener :: ( HWTInitAccessibleValue (Value a loc)
                 , HWTActionAccessibleValueLocation loc
                 , Data a)
              => Value a loc
              -> HWTAction ()
              -> HWT Listener
valueListener v a = withNewHWTId Nothing $ \i -> do
  addListener v (ValueListener a)
