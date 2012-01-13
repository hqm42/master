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
  projection p v = do
    v' <- newHWTId Nothing
    let
      steps = SU.join "," $ map (\(PS _ i) -> show i) $ execP p
      impl = "new hwt.project(pollingHandler.values," ++ show v ++ ",goog.array.concat(" ++ steps ++ "))"
    addConstructorCallConst (show v') impl
    return v'

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

readWriteModel :: (ModelReadableWriteableLocation loc, HWTPrefix (Value a loc), Data a)
               => Value a loc
               -> HWT (ReadWriteModel a loc)
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

negateModel :: ( ModelReadableLocation loc
               , HWTPrefix (ReadModel Bool loc)
               , MonadHWT m )
            => ReadModel Bool loc
            -> m (ReadModel Bool loc)
negateModel rm = withNewHWTId Nothing $ \i -> do
  addConstructorCallConst (show i) $ "new hwt.NegateModel(" ++ show rm ++ ")"

label :: MonadHWT m => ReadModel a loc -> m Element
label m = newElement $ \i -> do
  let
    impl = "new hwt.widgets.Label(" ++ (show m) ++ ")"
  addConstructorCallConst (show i) impl

labelC :: (Data a, MonadHWT m) => a -> m Element
labelC x = do
  m <- constModel x
  label m

textfield :: ReadWriteModel a loc -> HWT Element
textfield m = newElement $ \i -> do
  let
    impl = "new hwt.widgets.TextField(" ++ show m ++ ", null, null)"
  addConstructorCallConst (show i) impl

panel :: MonadHWT m => [Element] -> Maybe (ReadModel Bool loc) -> Maybe (ReadModel String loc) -> m Element
panel es vis cls = newElement $ \i -> do
  let
    impl = "new hwt.widgets.Panel(" ++ optModel vis ++ "," ++ optModel cls ++ ",goog.array.concat(" ++ SU.join "," (map show es) ++ "))"
  addConstructorCallConst (show i) impl

optModel :: HWTPrefix (Model a at loc) => Maybe (Model a at loc) -> String
optModel Nothing = "null"
optModel (Just m) = show m


copyButton :: ( MonadHWT m
              , HWTPrefix (Value b loc1)
              , HWTPrefix (Value b loc2))
           => ReadModel a loc
           -> Value b loc1
           -> Value b loc2
           -> m Element
copyButton m from to = newElement $ \i -> addConstructorCallConst (show i) impl
  where
    impl = "new hwt.widgets.Button(" ++ show m 
      ++ ",function(){" ++ show to ++ ".set(" ++ show from ++ ".get());},null)"

submitButton :: (Data a, Monoid a) => ReadModel a loc -> TransientValue a -> (a -> HWTAction ()) -> HWT Element
submitButton m tv a = do
  actionHandlerValue <- windowValue mempty
  valueListener actionHandlerValue a
  copyButton m tv actionHandlerValue

button :: ReadModel a loc -> (HWTAction ()) -> HWT Element
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
