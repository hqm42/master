
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE FlexibleContexts #-}
module Data.GenericDiffMap.Core 
  ( GDMap
  , GDMapValue(..)
  , newGDMap
  , Ref(..)
  , GRef
  , Updates(..)
  , UpdateHandler
  , InsertHandler
  , DeleteHandler
  , insert
  , update
  , delete
  , lookup
  , member
  , serialize
  , deserialize
  , handleDeserialize
  , handleInsert
  , handleUpdate
  , handleDelete)
  where

import Prelude hiding (lookup)
import Data.Data
import Data.Generics.Twins (geq)
import Data.Dynamic
import qualified Data.IntMap as M
import qualified Control.Monad.State as MS
import qualified Control.Monad.Identity as MI
import qualified Control.Monad.Writer as MW
import Data.Maybe (fromJust)
import Data.Monoid
import Control.Monad (when)

data DestructuredValue a b = Primitive {
  primitive :: a,
  serializePrimitive :: GDMap a b -> b,
  deserializePrimitive :: Monad m => b -> UpdateHandler m -> GDMap a b -> m (GDMap a b)
} | Complex  {
  constr :: Constr,
  childRefs :: [GRef],
  serializeComplex :: GDMap a b -> b,
  deserializeComplex :: Monad m => b -> UpdateHandler m -> InsertHandler m -> DeleteHandler m -> GDMap a b -> m (GDMap a b)
}

data GDMapValue a = GDPrimitive a | GDComplex String [GRef]

instance Show a => Show (DestructuredValue a b) where
  show Primitive{primitive=p} = show p
  show Complex{constr=c,childRefs=cs} = show c ++ " " ++ show cs

data Ref a = Ref {gRef :: Int} deriving (Eq,Ord,Typeable)
type GRef = Int

instance Show (Ref a) where
  show (Ref i) = show i

data GDMap g g' = GDMap {
  nextId :: GRef,
  dynMap :: M.IntMap (DestructuredValue g g'),
  stopRule :: forall a. Data a => a -> Bool,
  toG :: forall a. Data a => a -> g,
  fromG :: forall a. Data a => g -> Maybe a,
  toG' :: forall a. Data a => a -> g',
  fromG' :: forall a. Data a => g' -> Maybe a
}

newGDMap :: (forall a. Data a => a -> Bool)
         -> (forall a. Data a => a -> g)
         -> (forall a. Data a => g -> Maybe a)
         -> (forall a. Data a => a -> g')
         -> (forall a. Data a => g' -> Maybe a)
         -> GDMap g g'
newGDMap stopRule toPersistent fromPersistent serialize deserialize =
  GDMap 0 M.empty stopRule toPersistent fromPersistent serialize deserialize

data Updates = Updates {
  newValues :: [GRef],
  modifiedValues :: [GRef],
  deletedValues :: [GRef]
} deriving Show

instance Monoid Updates where
  mempty = Updates [] [] []
  mappend (Updates n m d) (Updates n' m' d') = Updates (n ++ n') (m ++ m') (d ++ d')

instance Show a => Show (GDMap a _a') where
  show GDMap{dynMap=dm} = "GDMap " ++ show dm


type UpdateHandler m = forall b. Data b => Monad m => Ref b -> b -> b -> GDMapValue b -> m ()

type InsertHandler m = forall b. Data b => Monad m => Ref b -> b -> GDMapValue b -> m ()

type DeleteHandler m = Monad m => GRef -> m ()

insert :: Data a => a -> GDMap g g' -> (Ref a,GDMap g g')
insert x m = MI.runIdentity (handleInsert x (\_ _ _ -> return ()) m)

-- replaces the current value with a new one. fails if type does not match, or value not present
update :: Data a => Ref a -> a -> GDMap g g' -> (Updates,GDMap g g')
update i x m = (us,m')
  where
    (m',us) = MW.runWriter (handleUpdate i x upd ins del m)
    upd = (\i _ _ _ -> MW.tell $ mempty{modifiedValues=[gRef i]})
    ins = (\i _ _ -> MW.tell $ mempty{newValues=[gRef i]})
    del = (\gi -> MW.tell $ mempty{deletedValues=[gi]})

delete :: Ref a -> GDMap g g' -> ([GRef],GDMap g g')
delete i m = (ds,m') 
  where
    (m',ds) = MW.runWriter (handleDelete (gRef i) (\gi -> MW.tell [gi]) m)

lookup :: Data a => Ref a -> GDMap g g' -> Maybe a
lookup i dm@GDMap{dynMap = m, stopRule = sr, fromG = from} = case M.lookup (gRef i) m of
  Just Primitive{primitive=d} -> case from d of
    Just x -> Just x
    Nothing -> error $ "GDiff.lookup: key " ++ show i ++ " :: (" ++ show (typeOf i) ++ ") cast failed (wrong type)"
  Just Complex{constr=con,childRefs=cs} -> Just $ MS.evalState (fromConstrM f con) cs
    where
      f :: (Data a) => MS.State [GRef] a
      f = do 
            (x:xs) <- MS.get
            MS.put xs
            return $ lookup' (Ref x) dm
  Nothing -> Nothing

lookup' i dm = fromJust $ lookup i dm

member :: GRef -> GDMap g g' -> Bool
member gi GDMap{dynMap=dm} = M.member gi dm

serialize :: GRef -> GDMap g g' -> g'
serialize gi m@GDMap{dynMap=dm} = case fromJust $ M.lookup gi dm of
  Primitive{serializePrimitive=r} -> r m
  Complex{serializeComplex=r} -> r m

deserialize :: GRef -> g' -> GDMap g g' -> GDMap g g'
deserialize gi gx m = MI.runIdentity (handleDeserialize gi gx upd ins del m)
  where
    upd = \_ _ _ _ -> return ()
    ins = \_ _ _ -> return ()
    del = \_ -> return ()

handleDeserialize :: Monad m => GRef -> g' -> UpdateHandler m -> InsertHandler m -> DeleteHandler m -> GDMap g g' -> m (GDMap g g')
handleDeserialize gi gx upd ins del m@GDMap{dynMap=dm} = case fromJust $ M.lookup gi dm of
  Primitive{deserializePrimitive=d} -> d gx upd m
  Complex{deserializeComplex=d} -> d gx upd ins del m

ser :: Data a => (Ref a) -> GDMap g g' -> g'
ser i m = (toG' m) $ lookup' i m

desP :: (Data a,Monad m') => Ref a ->  g' -> UpdateHandler m' -> GDMap g g' -> m' (GDMap g g')
desP i x upd m = handleUpdate i (fromJust $ (fromG' m) x) upd undefined undefined m

desC :: (Data a,Monad m') => Ref a -> g' -> UpdateHandler m' -> InsertHandler m' -> DeleteHandler m' -> GDMap g g' -> m' (GDMap g g')
desC i x upd ins del m = handleUpdate i (fromJust $ (fromG' m) x) upd ins del m 

handleUpdate :: (Monad m, Data a) 
              => Ref a
              -> a
              -> (forall b. Data b => Ref b -> b -> b -> GDMapValue b -> m ()) -- update
              -> (forall b. Data b => Ref b -> b -> GDMapValue b -> m ()) -- insert
              -> (GRef -> m ()) -- delete
              -> GDMap g g'
              -> m (GDMap g g')
handleUpdate i x' update insert delete = MS.execStateT (handleUpdate' i x' update insert delete)

handleUpdate' :: (Monad m, Data a)
               => Ref a
               -> a
               -> (forall b. Data b => Ref b -> b -> b -> GDMapValue b -> m ()) -- update
               -> (forall b. Data b => Ref b -> b -> GDMapValue b -> m ()) -- insert
               -> (GRef -> m ()) -- delete
               -> MS.StateT (GDMap g g') m ()
handleUpdate' i x' update insert delete = do
  m <- MS.get
  let
    x = lookup' i m
    sr = stopRule m
    gi = gRef i
    dm = dynMap m
    to = toG m
  if x `geq` x' then
    return ()
  else if sr x' then do
    MS.lift $ update i x x' (GDPrimitive x') -- side effects
    MS.put m{dynMap=M.insert gi Primitive{ primitive=to x'
                                         , serializePrimitive=ser i
                                         , deserializePrimitive=desP i} dm}
  else if toConstr x' == constr (fromJust $ M.lookup gi dm) then do
    res <- MS.execStateT (sequence $ gmapQ (\y' -> do
                    (yr:yrs) <- MS.get
                    MS.lift $ handleUpdate' (Ref yr) y' update insert delete
                    MS.put yrs) x') (childRefs $ fromJust $ M.lookup gi dm)
    return ()
  else do
    mapM_ (\c -> handleDelete' c delete) (childRefs $ fromJust $ M.lookup gi dm)
    handleInsertI' i x' insert
    Complex{constr=c, childRefs=cs} <- MS.gets (fromJust . M.lookup gi . dynMap)
    MS.lift $ update i x x' (GDComplex (show c) cs)
    return ()

handleInsert :: (Monad m, Data a)
             => a
             -> (forall b. Data b => Ref b -> b -> GDMapValue b -> m ())
             -> GDMap g g'
             -> m (Ref a,GDMap g g')
handleInsert x insert m = do
  (rs,m) <- MS.runStateT (handleInsert' x insert) m
  return (Ref $ head rs,m)

handleInsert' :: (Monad m, Data a)
              => a
              -> (forall b. Data b => Ref b -> b -> GDMapValue b -> m ())
              -> MS.StateT (GDMap g g') m [GRef]
handleInsert' x insert = do
  i <- MS.gets nextId
  let
    newRef = Ref i
  MS.modify (\m -> m{nextId=i+1})
  cs <- handleInsertI' newRef x insert
  gdv' <- MS.gets $ fromJust . M.lookup i . dynMap
  from <- MS.gets $ fromG
  let
    gdv = case gdv' of
            Primitive{primitive=x} -> GDPrimitive $ fromJust . from $ x
            Complex{constr=c,childRefs=cs}-> GDComplex (show c) cs
  MS.lift $ insert newRef x gdv
  return $ gRef newRef : cs

handleInsertI' :: (Monad m, Data a)
               => Ref a
               -> a
               -> (forall b. Data b => Ref b -> b -> GDMapValue b -> m ())
               -> MS.StateT (GDMap g g') m [GRef]
handleInsertI' i x insert = do
  m <- MS.get
  let
    sr = stopRule m
    dm = dynMap m
    to = toG m
    gi = gRef i
  if sr x then do
    let
      prim = Primitive{ primitive = to x
                      , serializePrimitive = ser i
                      , deserializePrimitive = desP i} 
    MS.put m{dynMap=M.insert (gRef i) prim dm}
    return []
  else do
    childrens <- sequence $ gmapQ (\y -> handleInsert' y insert) x
    m <- MS.get
    let
      children = map head childrens
      dm = dynMap m
    MS.modify $ \m -> m{dynMap = M.insert (gRef i) Complex{ constr=toConstr x
                                                          , childRefs=children
                                                          , serializeComplex=ser i
                                                          , deserializeComplex=desC i}
                                                          dm}
    return $ concat childrens

handleDelete :: (Monad m)
             => GRef
             -> (GRef -> m ())
             -> GDMap g g'
             -> m (GDMap g g')
handleDelete gi delete m = MS.execStateT (handleDelete' gi delete) m

handleDelete' :: (Monad m)
              => GRef
              -> (GRef -> m ())
              -> MS.StateT (GDMap g g') m ()
handleDelete' gi delete = do
  m <- MS.get
  let
    dm = dynMap m
    dv = fromJust $ M.lookup gi dm
  MS.put m{dynMap = M.delete gi dm}
  case dv of
    Primitive{} -> return ()
    Complex{childRefs=cs} -> mapM_ (\c -> handleDelete' c delete) cs
  MS.lift $ delete gi
