{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE FlexibleContexts #-}
module GDiff where

import Prelude hiding (lookup)
import Text.JSON.Generic
import Data.Data
import Data.Generics.Twins (geq)
import Data.Generics.Text
import Data.Dynamic
import qualified Data.Map as M
import qualified Control.Monad.State as MS
import qualified Control.Monad.Identity as MI
import qualified Control.Monad.Writer as MW
import Data.Maybe (fromJust)
import Data.Monoid
import Control.Monad (when)

data Foo = Foo1 {
  bar1 :: Int,
  baz1 :: [String]
} | Foo2 {
  bar2 :: Float,
  baz2 :: [Int]
} deriving (Eq,Show,Typeable,Data)

foo = Foo1 42 ["Hallo","Welt"]

data DestructuredValue = Primitive {
  primitive :: Dynamic
} | Complex  {
  constr :: Constr,
  childRefs :: [GRef]
} deriving Show

data Ref a = Ref {gRef :: Integer} deriving (Eq,Ord)
type GRef = Integer

instance Show (Ref a) where
  show (Ref i) = show i

data DMap = DMap {
  nextId :: Integer,
  dynMap :: M.Map GRef DestructuredValue,
  stopRule :: forall a. Data a => a -> Bool
}

data Updates = Updates {
  newValues :: [GRef],
  modifiedValues :: [GRef],
  deletedValues :: [GRef]
} deriving Show

instance Monoid Updates where
  mempty = Updates [] [] []
  mappend (Updates n m d) (Updates n' m' d') = Updates (n ++ n') (m ++ m') (d ++ d')

instance Show DMap where
  show DMap{dynMap=dm} = "DMap " ++ show dm


type UpdateHandler m = forall b. Data b => Monad m => Ref b -> b -> b -> m ()

type InsertHandler m = forall b. Data b => Monad m => Ref b -> b -> m ()

type DeleteHandler m = Monad m => GRef -> m ()

insert :: Data a => a -> DMap -> (Ref a,DMap)
insert x m = MI.runIdentity (handleInsert x (\_ _ -> return ()) m)

-- replaces the current value with a new one. fails if type does not match, or value not present
update :: Data a => Ref a -> a -> DMap -> (Updates,DMap)
update i x m = (us,m')
  where
    (m',us) = MW.runWriter (handleUpdates i x upd ins del m)
    upd = (\i _ _ -> MW.tell $ mempty{modifiedValues=[gRef i]})
    ins = (\i _ -> MW.tell $ mempty{newValues=[gRef i]})
    del = (\gi -> MW.tell $ mempty{deletedValues=[gi]})

delete :: Ref a -> DMap -> ([GRef],DMap)
delete i m = (ds,m') 
  where
    (m',ds) = MW.runWriter (handleDelete (gRef i) (\gi -> MW.tell [gi]) m)

lookup :: Data a => Ref a -> DMap -> a
lookup i dm@DMap{dynMap = m, stopRule = sr} = case M.lookup (gRef i) m of
  Just (Primitive d) -> fromJust $ fromDynamic d
  Just (Complex con cs) -> MS.evalState (fromConstrM f con) cs
    where
      f :: (Data a) => MS.State [GRef] a
      f = do 
            (x:xs) <- MS.get
            MS.put xs
            return $ lookup (Ref x) dm
  Nothing -> error "not found"

handleUpdates :: (Monad m, Data a) 
              => Ref a
              -> a
              -> (forall b. Data b => Ref b -> b -> b -> m ()) -- update
              -> (forall b. Data b => Ref b -> b -> m ()) -- insert
              -> (GRef -> m ()) -- delete
              -> DMap
              -> m DMap
handleUpdates i x' update insert delete = MS.execStateT (handleUpdates' i x' update insert delete)

handleUpdates' :: (Monad m, Data a)
               => Ref a
               -> a
               -> (forall b. Data b => Ref b -> b -> b -> m ()) -- update
               -> (forall b. Data b => Ref b -> b -> m ()) -- insert
               -> (GRef -> m ()) -- delete
               -> MS.StateT DMap m ()
handleUpdates' i x' update insert delete = do
  m <- MS.get
  let
    x = lookup i m
    sr = stopRule m
    gi = gRef i
    dm = dynMap m
  if x `geq` x' then
    return ()
  else if sr x' then do
    MS.lift $ update i x x' -- side effects
    MS.put m{dynMap=M.insert gi (Primitive $ toDyn x') dm}
  else if toConstr x' == constr (fromJust $ M.lookup gi dm) then do
    res <- MS.execStateT (sequence $ gmapQ (\y' -> do
                    (yr:yrs) <- MS.get
                    MS.lift $ handleUpdates' (Ref yr) y' update insert delete
                    MS.put yrs) x') (childRefs $ fromJust $ M.lookup gi dm)
    return ()
  else do
    mapM_ (\c -> handleDelete' c delete) (childRefs $ fromJust $ M.lookup gi dm)
    handleInsertI' i x' insert
    MS.lift $ update i x x'
    return ()

handleInsert :: (Monad m, Data a)
             => a
             -> (forall b. Data b => Ref b -> b -> m ())
             -> DMap
             -> m (Ref a,DMap)
handleInsert x insert m = do
  (rs,m) <- MS.runStateT (handleInsert' x insert) m
  return (Ref $ head rs,m)

handleInsert' :: (Monad m, Data a)
              => a
              -> (forall b. Data b => Ref b -> b -> m ())
              -> MS.StateT DMap m [GRef]
handleInsert' x insert = do
  i <- MS.gets nextId
  let
    newRef = Ref i
  MS.modify (\m -> m{nextId=i+1})
  cs <- handleInsertI' newRef x insert
  MS.lift $ insert newRef x
  return $ gRef newRef : cs

handleInsertI' :: (Monad m, Data a)
               => Ref a
               -> a
               -> (forall b. Data b => Ref b -> b -> m ())
               -> MS.StateT DMap m [GRef]
handleInsertI' i x insert = do
  m <- MS.get
  let
    sr = stopRule m
    dm = dynMap m
  if sr x then do
    MS.put m{dynMap=M.insert (gRef i) (Primitive $ toDyn x) dm}
    return []
  else do
    childrens <- sequence $ gmapQ (\y -> handleInsert' y insert) x
    m <- MS.get
    let
      children = map head childrens
      dm = dynMap m 
    MS.modify $ \m -> m{dynMap = M.insert (gRef i) (Complex (toConstr x) children) dm}
    return $ concat childrens

handleDelete :: (Monad m)
             => GRef
             -> (GRef -> m ())
             -> DMap
             -> m DMap
handleDelete gi delete m = MS.execStateT (handleDelete' gi delete) m

handleDelete' :: (Monad m)
              => GRef
              -> (GRef -> m ())
              -> MS.StateT DMap m ()
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

debugUpdate :: UpdateHandler IO
debugUpdate = \ref old new -> putStrLn $ show ref ++ " updated: " ++ gshow old ++ " -> " ++  gshow new

debugInsert :: InsertHandler IO
debugInsert = \ref inserted -> putStrLn $ show ref ++ " inserted: " ++ gshow inserted

debugDelete :: DeleteHandler IO
debugDelete = \gref -> putStrLn $ show gref ++ " deleted"

handleExamle :: IO DMap
handleExamle = handleUpdates (Ref 0) (2 :: Int) debugUpdate undefined undefined $
        snd $ insert (1 :: Int) empty

handleExamle2 :: IO DMap
handleExamle2 = handleUpdates (Ref 0) foo{bar1=11, baz1=["Hallo","Uwe"]} debugUpdate undefined undefined $
        snd $ insert foo empty

handleExamle3 :: IO DMap
handleExamle3 = handleUpdates (Ref 0) (Foo2 4.2 [1]) debugUpdate debugInsert debugDelete $
            snd $ insert foo empty

defaultStopRule :: forall a . Data a => a -> Bool
defaultStopRule = isAlgType . dataTypeOf

betterStopRule :: forall a . Data a => a -> Bool
betterStopRule x = not (isAlgType dtx) || (tx == typeOf "")
  where
    dtx = dataTypeOf x
    tx = typeOf x

empty = DMap{ nextId = 0
            , dynMap = M.empty
            , stopRule = betterStopRule}


test1@(fooRef,dm1) = insert foo empty
test2@(us1,dm2) = update fooRef foo{bar1=11} dm1
test3@(us2,dm3) = delete fooRef dm2
