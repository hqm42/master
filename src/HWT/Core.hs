{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HWT.Core where

import HWT.Types
import HWT.Session
import qualified Data.GenericDiffMap as GDM
import Data.GenericDiffMap.Projection

import Control.Monad.State
import Control.Monad.Reader
import Data.Data
import Text.JSON.Generic
import Text.JSON.Pretty
import qualified Data.IntMap as IM
import qualified Data.String.Utils as SU




newHWTInit :: HWTInit
newHWTInit = HWTInit { nextId = 0
                     , constructorCalls = []
                     , initialServerValues = GDM.newJSONGDMap
                     , initialClientValues = GDM.newJSONGDMap
                     , initialWindowValues = GDM.newJSONGDMap
                     , initialServerValueListeners = IM.empty
                     , initialClientValueListeners = IM.empty
                     , initialWindowValueListeners = IM.empty}

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
addConstructorCall refName impl = addConstructorCallPR refName (return impl)

addConstructorCallPR :: String -> HWTPageReload String -> HWT ()
addConstructorCallPR refName implAction = modify $ \ini@HWTInit{constructorCalls=cc} -> ini{constructorCalls=cc++[cv]}
  where
    cv = ConstructorCall refName implAction


toJSON2 (GDM.GDPrimitive json) = json
toJSON2 c = complex2JSON c

serverValue :: Data a => a -> HWT (ServerValue a)
serverValue x = do
  icvs <- gets initialServerValues
  (ref,isvs') <- GDM.handleInsert x (\r _ _ -> serverValue' (ref2Value r)) icvs
  modify $ \ini -> ini{initialServerValues=isvs'}
  return (ref2Value ref)
    
serverValue' :: Data a => ServerValue a -> HWT ()
serverValue' i = addConstructorCallPR refName implA
  where
    refName = show i
    implA = do
      svs <- asks pageReloadServerValues
      let
        gdvM = GDM.lookupGDMapValue (GDM.gRef $ value2Ref i) svs
      let
        js = case gdvM of
          Nothing -> error "Value not found! 'serverValue''"
          Just gdv -> renderJSON $ toJSON2 gdv
      return $ "new hwt.ServerValue(pollingHandler," ++ js ++ ",'" ++ refName ++ "')"

clientValue :: Data a => a -> HWT (ClientValue a)
clientValue x = do
  icvs <- gets initialClientValues
  (ref,icvs') <- GDM.handleInsert x (\r _ gdmv -> clientValue' (ref2Value r)) icvs
  modify $ \ini -> ini{initialClientValues=icvs'}
  return (ref2Value ref)

clientValue' :: Data a => ClientValue a -> HWT ()
clientValue' i = addConstructorCallPR refName implA
  where
    refName = show i
    implA = do
      cvs <- asks pageReloadClinetValues
      let
        gdvM = GDM.lookupGDMapValue (GDM.gRef $ value2Ref i) cvs
      let
        js = case gdvM of
          Nothing -> error "Value not found! 'clientValue''"
          Just gdv -> renderJSON $ toJSON2 gdv
      return $ "new hwt.Value(pollingHandler," ++ js ++ ",'" ++ refName ++ "')"

windowValue :: Data a => a -> HWT (WindowValue a)
windowValue x = do
  iwvs <- gets initialWindowValues
  (ref,iwvs') <- GDM.handleInsert x (\r _ gdmv -> windowValue' (ref2Value r) gdmv) iwvs
  modify $ \ini -> ini{initialWindowValues=iwvs'}
  return (ref2Value ref)

windowValue' :: Data a => WindowValue a -> GDM.GDMapValue a -> HWT ()
windowValue' i gdv = addConstructorCall refName impl
  where
    refName = show i
    js = renderJSON $ gdv2JSON gdv
    impl = "new hwt.Value(pollingHandler," ++ js ++ ",'" ++ refName ++ "')"

transientValue :: Data a => a -> HWT (TransientValue a)
transientValue x = withNewHWTId Nothing $ \i -> do
  let
    refName = show i
    js = renderJSON $ toJSON x
    impl = "new hwt.TransientValue(pollingHandler," ++ js ++ ",'" ++ refName ++ "')"
  addConstructorCall refName impl

model' :: ( HWTPrefix (Model a at loc)
          , HWTPrefix (Value a loc))
       => (Model a at loc)
       -> HWT ()
model' m@(HWTId _ (Just v)) = do
  let
    impl = "new hwt.Model" ++ getPrefix m ++ "(" ++ show v ++ ")"
    refName = show m
  addConstructorCall refName impl
  return ()

readModel :: (ModelReadableLocation loc, HWTPrefix (Value a loc), Data a)
          => Value a loc
          -> HWT (ReadModel a loc)
readModel v = withNewHWTId (Just v) model'

readWriteModel :: (ModelReadableWriteableLocation loc, HWTPrefix (Value a loc), Data a)
               => Value a loc
               -> HWT (ReadWriteModel a loc)
readWriteModel v = withNewHWTId (Just v) model'

writeModel :: (ModelWriteableLocation loc, HWTPrefix (Value a loc), Data a)
           => Value a loc
           -> HWT (WriteModel a loc)
writeModel v = withNewHWTId (Just v) model'

newElement = withNewHWTId Nothing

label :: ReadModel a loc -> HWT Element
label m = newElement $ \i -> do
  let
    impl = "new hwt.widgets.Label(" ++ (show m) ++ ")"
  addConstructorCall (show i) impl

textfield :: ReadWriteModel a loc -> HWT Element
textfield m = newElement $ \i -> do
  let
    impl = "new hwt.widgets.TextField(" ++ show m ++ ", null, null)"
  addConstructorCall (show i) impl

panel :: [Element] -> HWT Element
panel es = newElement $ \i -> do
  let
    impl = "new hwt.widgets.Panel(null,null,new Array(" ++ SU.join "," (map show es) ++ "))"
  addConstructorCall (show i) impl

button :: ReadModel a loc -> HWTAction () -> HWT Element
button m a = do
  actionHandlerValue <- windowValue ""
  valueListener actionHandlerValue a
  newElement $ \i -> do
    let
      impl = "new hwt.widgets.Button(" ++ show m ++ ",function(){" ++ show actionHandlerValue ++ ".set('');},null)"
    addConstructorCall (show i) impl

listReadModels :: ( Data a
                  , HWTPrefix (Value a loc)
                  , HWTPrefix (Value [a] loc)
                  , HWTInitAccessibleValue (Value [a] loc))
               => ReadModel [a] loc
               -> HWT [ReadModel a loc]
listReadModels m@(HWTId _ (Just v)) = do
  c <- withInitSessionMap v $ \sm -> case GDM.lookup (value2Ref v) sm of
    Nothing -> error "Value not found 'listReadModels'"
    Just c -> return c
  case c of
    [] -> return []
    _ -> do
      em <- projection h m
      m' <- projection t m
      ems <- listReadModels m'
      return $ em:ems
  where
    h = pa head $ PS "Liste-bla" 0
    t = pa tail $ PS "Liste-bla" 1
  

list :: ( Data a
        , HWTPrefix (Value a loc)
        , HWTPrefix (Value [a] loc)
        , HWTInitAccessibleValue (Value a loc)
        , HWTInitAccessibleValue (Value [a] loc))
     => ReadModel [a] loc -> (ReadModel a loc -> HWT Element) -> HWT Element
list m@(HWTId _ (Just v)) subwidget = do
  newElement $ \i -> do
    ms <- listReadModels m
    es <- mapM subwidget ms
    let
      impl = "new hwt.widgets.Panel(null,null,new Array(" ++ SU.join "," (map show es) ++ "))"
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
  withInitSessionMap v $ \sm -> withNewHWTId (Just $ ref2Value $ GDM.projection p (value2Ref v) sm) $ model' 

valueListener :: ( HWTInitAccessibleValue (Value a loc)
                 , HWTActionAccessibleValueLocation loc
                 , Data a)
              => Value a loc
              -> HWTAction ()
              -> HWT Listener
valueListener v a = withNewHWTId Nothing $ \i -> do
  addListener v (ValueListener a)
