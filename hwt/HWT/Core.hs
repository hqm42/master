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
import Control.Monad.Writer
import Data.Data
import Data.Maybe
import Text.JSON.Generic
import Text.JSON.Pretty
import qualified Data.IntMap as IM
import qualified Data.String.Utils as SU
import Control.Concurrent.AdvSTM.TVar

instance MonadHWT HWT where
  addConstructorCalls cc =
    modify $ \ini@HWTInit{constructorCalls=ccs} -> ini{constructorCalls=ccs++[cc]}
  newHWTId meta = do
    i <- gets nextId
    modify $ \ini -> ini{nextId = i + 1}
    let
      res = HWTId i meta
    return $ res
  getWidgetContext = do
    HWTInit{ initialServerValues=svs
           , initialClientValues=cvs
           , initialWindowValues=wvs} <- get
    return $ HWTWidgetContext{ widgetServerValues=svs
                             , widgetClientValues=cvs
                             , widgetWindowValues=wvs}

instance ProjectionMonadHWT HWT where
  projection p v = do
    v' <- withInitSessionMap v $ 
      \sm -> return $ ref2Value $ GDM.projection p (value2Ref v) sm
    let
      steps = SU.join "," $ map (\(PS _ i) -> show i) $ execP p
      impl = "new hwt.project(pollingHandler.values," ++ show v ++ ",goog.array.concat(" ++ steps ++ "))"
    addConstructorCallConst (show v') impl
    return v'

instance MonadHWT HWTWidget where
  addConstructorCalls cc = tell [cc]
  newHWTId meta = do
    i <- get
    put $ i + 1
    return $ HWTId i meta
  getWidgetContext = ask

instance ProjectionMonadHWT HWTWidget where
  projection p v = do
    v' <- newHWTId Nothing
    let
      steps = SU.join "," $ map (\(PS _ i) -> show i) $ execP p
      impl = "new hwt.project(pollingHandler.values," ++ show v ++ ",goog.array.concat(" ++ steps ++ "))"
    addConstructorCallConst (show v') impl
    return v'

instance ProjectionMonadHWT HWTAction where
  projection p v = withSessionMap v $ \smT -> do
    sm <- readTVar smT
    let
      r' = GDM.projection p (value2Ref v) sm
    return $ ref2Value r'

withInitSessionMap :: ( MonadHWT m
                      , HWTInitAccessibleValue a)
                   => a
                   -> (SessionMap -> m b)
                   -> m b
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

addConstructorCallConst :: MonadHWT m => String -> String -> m ()
addConstructorCallConst ref impl = addConstructorCalls $ return [ConstructorCall ref impl]

newHWTInit :: HWTInit
newHWTInit = HWTInit { nextId = 0
                     , constructorCalls = []
                     , initialServerValues = GDM.newJSONGDMap
                     , initialClientValues = GDM.newJSONGDMap
                     , initialWindowValues = GDM.newJSONGDMap
                     , initialServerValueListeners = IM.empty
                     , initialClientValueListeners = IM.empty
                     , initialWindowValueListeners = IM.empty}

withNewHWTId :: MonadHWT m
             => Maybe b -> (HWTId a b -> m ()) -> m (HWTId a b)
withNewHWTId meta a = do
  i <- newHWTId meta
  a i
  return i

newElement :: MonadHWT m => (Element -> m ()) -> m Element
newElement a = withNewHWTId Nothing a

renderJSON :: JSValue -> String
renderJSON = render . pp_value

serverValue :: Data a => a -> HWT (ServerValue a)
serverValue x = do
  isvs <- gets initialServerValues
  let
    (ref,isvs') = GDM.insert x isvs
  modify $ \ini -> ini{initialServerValues=isvs'}
  addConstructorCalls $ do
    svs <- asks pageReloadServerValues
    let
      serverValue' r x _ = do
                             let
                               gdvM = GDM.lookupGDMapValue (GDM.gRef r) svs
                               sv = ref2GValue r :: ServerValue ()
                               refName = show sv
                             case gdvM of
                               Nothing -> error "Value not found! 'serverValue'"
                               Just gdv -> tell [ConstructorCall refName impl]
                                 where
                                   js = renderJSON $ toJSON2 sv gdv
                                   impl = "new hwt.ServerValue(pollingHandler," ++ js ++ ",'" ++ refName ++ "')"
                             return x
    return $ execWriter (GDM.handleLookup ref serverValue' svs)
  return (ref2Value ref)
    
clientValue :: Data a => a -> HWT (ClientValue a)
clientValue x = do
  icvs <- gets initialClientValues
  let
    (ref,icvs') = GDM.insert x icvs
  modify $ \ini -> ini{initialClientValues=icvs'}
  addConstructorCalls $ do
    cvs <- asks pageReloadClinetValues
    let
      clientValue' r x _ = do
                             let
                               gdvM = GDM.lookupGDMapValue (GDM.gRef r) cvs
                               cv = ref2GValue r :: ClientValue ()
                               refName = show cv
                             case gdvM of
                               Nothing -> error "Value not found! 'clientValue'"
                               Just gdv -> tell [ConstructorCall refName impl]
                                 where
                                   js = renderJSON $ toJSON2 cv gdv
                                   impl = "new hwt.ServerValue(pollingHandler," ++ js ++ ",'" ++ refName ++ "')"
                             return x
    return $ execWriter (GDM.handleLookup ref clientValue' cvs)
  return (ref2Value ref)

windowValue :: Data a => a -> HWT (WindowValue a)
windowValue x = do
  iwvs <- gets initialWindowValues
  (ref,iwvs') <- GDM.handleInsert x (\r _ gdmv -> windowValue' (ref2Value r) gdmv) iwvs
  modify $ \ini -> ini{initialWindowValues=iwvs'}
  return (ref2Value ref)

windowValue' :: Data a => WindowValue a -> GDM.GDMapValue a -> HWT ()
windowValue' i gdv = addConstructorCallConst refName impl
  where
    refName = show i
    gdv' = case gdv of
      GDM.GDPrimitive p -> GDM.GDPrimitive $ toJSON p
      GDM.GDComplex cn cs -> GDM.GDComplex cn cs
    js = renderJSON $ toJSON2 i gdv'
    impl = "new hwt.Value(pollingHandler," ++ js ++ ",'" ++ refName ++ "')"

transientValue :: (Data a, MonadHWT m) => a -> m (TransientValue a)
transientValue x = withNewHWTId Nothing $ \i -> do
  let
    refName = show i
    js = renderJSON $ toJSON x
    impl = "new hwt.TransientValue(pollingHandler," ++ js ++ ",'" ++ refName ++ "')"
  addConstructorCallConst refName impl

model' :: ( HWTPrefix (Model a at loc)
          , HWTPrefix (Value a loc)
          , MonadHWT m)
       => (Model a at loc)
       -> m ()
model' m@(HWTId _ (Just v)) = do
  let
    impl = "new hwt.Model" ++ getPrefix m ++ "(" ++ show v ++ ")"
    refName = show m
  addConstructorCallConst refName impl
  return ()

readModel :: ( ModelReadableLocation loc
             , HWTPrefix (Value a loc)
             , Data a
             , MonadHWT m)
          => Value a loc
          -> m (ReadModel a loc)
readModel v = withNewHWTId (Just v) model'

readWriteModel :: (ModelReadableWriteableLocation loc, HWTPrefix (Value a loc), Data a, MonadHWT m)
               => Value a loc
               -> m (ReadWriteModel a loc)
readWriteModel v = withNewHWTId (Just v) model'

writeModel :: (ModelWriteableLocation loc, HWTPrefix (Value a loc), Data a)
           => Value a loc
           -> HWT (WriteModel a loc)
writeModel v = withNewHWTId (Just v) model'

constModel :: ( Data a
              , MonadHWT m)
           => a
           -> m (ReadModel a TransientLocation)
constModel x = do
  tv <- transientValue x
  readModel tv

negateModel :: ( HasModel ms Bool loc
               , ModelReadableLocation loc
               , HWTPrefix (ReadModel Bool loc)
               , MonadHWT m )
            => ms
            -> m (ReadModel Bool loc)
negateModel ms = withNewHWTId Nothing $ \i -> do
  rm <- getReadModel ms
  addConstructorCallConst (show i) $ "new hwt.NegateModel(" ++ show rm ++ ")"

eqModel :: ( MonadHWT m
           , HasModel ms1 a loc1
           , HasModel ms2 a loc2
           , HWTPrefix (ReadModel a loc1)
           , HWTPrefix (ReadModel a loc2))
        => ms1 -> ms2 -> m (ReadModel Bool loc1)
eqModel ms1 ms2 = do
  withNewHWTId Nothing $ \i -> do
  m1 <- getReadModel ms1
  m2 <- getReadModel ms2
  addConstructorCallConst (show i) $ "new hwt.EqualsModel(" ++ show m1 ++ "," ++ show m2 ++ ")"

label :: (MonadHWT m, HasModel ms a loc) => ms -> m Element
label m = newElement $ \i -> do
  m' <- getReadModel m
  let
    impl = "new hwt.widgets.Label(" ++ (show m') ++ ")"
  addConstructorCallConst (show i) impl

textfield :: MonadHWT m => ReadWriteModel a loc -> m Element
textfield m = newElement $ \i -> do
  let
    impl = "new hwt.widgets.TextField(" ++ show m ++ ", null, null)"
  addConstructorCallConst (show i) impl

textfieldT :: (MonadHWT m) => String -> m (Element,TransientValue String)
textfieldT t = do
  tv <- transientValue t
  m <- readWriteModel tv
  t <- textfield m
  return (t,tv)

panel :: ( MonadHWT m
         , HasModel ms1 Bool loc1
         , HasModel ms2 String loc2)
      => [Element] -> ms1 -> ms2 -> m Element
panel es vis cls = newElement $ \i -> do
  visM <- getReadModel vis
  clsM <- getReadModel cls
  let
    impl = "new hwt.widgets.Panel(" ++ show visM ++ "," ++ show clsM ++ ",goog.array.concat(" ++ SU.join "," (map show es) ++ "))"
  addConstructorCallConst (show i) impl

panel' es = panel es True "" 

copyButton :: ( MonadHWT m
              , HWTPrefix (Value b loc1)
              , HWTPrefix (Value b loc2)
              , HasModel ms a loc)
           => ms
           -> Value b loc1
           -> Value b loc2
           -> m Element
copyButton m from to = newElement $ \i -> do
  m' <- getReadModel m
  let
    impl = "new hwt.widgets.Button(" ++ show m'
      ++ ",function(){" ++ show to ++ ".set(" ++ show from ++ ".get());},null)"
  addConstructorCallConst (show i) impl

submitButton :: (Data a, Data b, Monoid b, HasModel ms a loc)
             => ms -> TransientValue b -> (b -> HWTAction ()) -> HWT Element
submitButton m tv a = do
  actionHandlerValue <- windowValue mempty
  valueListener actionHandlerValue a
  copyButton m tv actionHandlerValue

button :: HasModel ms a loc => ms -> (HWTAction ()) -> HWT Element
button m action = do
  actionHandlerValue <- windowValue 0
  actionValue <- transientValue 1
  valueListener actionHandlerValue $ \i -> do
    setValue actionValue $ i + (1 :: Int)
    action
  copyButton m actionValue actionHandlerValue

list :: ( Data a
        , HWTPrefix (Value a loc)
        , HWTPrefix (Value [a] loc)
        , HWTInitAccessibleValue (Value a loc)
        , HWTInitAccessibleValue (Value [a] loc)
        , MonadHWT m)
     => Value [a] loc
     -> (Value a loc -> HWTWidget Element)
     -> m Element
list lv subwidget = newElement $ \i -> do
  widgetContext <- getWidgetContext
  wv@(HWTId nid _) <- newHWTId Nothing
  let
    (widget,ccs') = runReader (evalStateT (runWriterT (subwidget wv)) (nid+1)) widgetContext
    ccs = concat $ map (\cc -> runReader (renderConstructorCalls cc) undefined) ccs'
    impl = "new hwt.widgets.List(new hwt.ListModelmR(" ++ show lv ++ ")"
         ++ ",function(" ++ show wv ++ "){\n" ++ ccs ++ "return " ++ show widget ++ ";})"
  addConstructorCallConst (show i) impl

valueListener :: ( HWTInitAccessibleValue (Value a loc)
                 , HWTActionAccessibleValueLocation loc
                 , Data a)
              => Value a loc
              -> (a -> HWTAction ())
              -> HWT Listener
valueListener v a = withNewHWTId Nothing $ \i -> do
  addListener v (ValueListener (a . (\(Ok x) -> x) . fromJSON))

instance HasModel (ReadModel a loc) a loc where
  getReadModel m = return m

instance ( ModelReadableLocation loc
         , HWTPrefix (Value a loc)
         , Data a)
        => HasModel (Value a loc) a loc where
  getReadModel = readModel

instance HasModel String String TransientLocation where
  getReadModel = constModel

instance HasModel Bool Bool TransientLocation where
  getReadModel = constModel

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
