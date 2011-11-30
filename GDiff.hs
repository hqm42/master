{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module GDiff where

import Prelude hiding (lookup)
import Text.JSON.Generic
import Data.Data
import Data.Generics.Twins (geq)
import Data.Dynamic
import qualified Data.Map as M
import qualified Control.Monad.State as MS
import Data.Maybe (fromJust)
import Data.Monoid
import Control.Monad (when)

data Foo = Foo1 {
  bar1 :: Int,
  baz1 :: [String]
} | Foo2 {
  bar2 :: Float,
  baz2 :: [Int]
}deriving (Eq,Show,Typeable,Data)

foo = Foo1 42 ["Hallo","Welt"]


data Foo3 = Foo31 Int 
          | Foo32 Int
          deriving (Eq,Show,Typeable,Data)

data DestructuredValue = Primitive {
  primitive :: Dynamic
} | Complex  {
  constr :: Constr,
  childRefs :: [GRef]
} deriving Show

data Ref a = Ref Integer deriving (Eq,Ord)
type GRef = Ref ()

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

gRef :: Ref a -> Ref b
gRef (Ref i) = Ref i

insert :: Data a => a -> DMap -> (Ref a,DMap)
insert x m = (gRef $ head is, dm)
  where
    (is,dm) = MS.runState (insert' x) m

insert' :: Data a => a -> MS.State DMap [GRef]
insert' x = do
  i <- MS.gets nextId
  let
    newRef = Ref i
  MS.modify (\m -> m{nextId=i+1})
  cs <- inserti' newRef x
  return $ newRef : cs

inserti' :: Data a => Ref a -> a -> MS.State DMap [GRef]
inserti' i x = do
  let
    gi = gRef i
  stopRule <- MS.gets stopRule
  if stopRule x
    then do
        MS.modify $ \dm -> dm{dynMap = M.insert gi (Primitive $ toDyn x) (dynMap dm)}
        return []
    else do
        childrens <- sequence $ gmapQ insert' x
        let
          children = map head childrens
        MS.modify $ \dm -> dm{dynMap = M.insert gi (Complex (toConstr x) children) (dynMap dm)}
        return $ concat childrens


-- replaces the current value with a new one. fails if type does not match, or value not present
update :: Data a => Ref a -> a -> DMap -> (Updates,DMap)
update i x = MS.runState (update' i x)

update' :: Data a => Ref a -> a -> MS.State DMap Updates
update' i x = do
  let
    gi = gRef i
  m@DMap{stopRule=sr,dynMap=dm} <- MS.get
  let
    oldValue = lookup i m
  if x `geq` oldValue then
    return mempty
    else if sr x then do
      MS.put m{dynMap=M.insert gi (Primitive $ toDyn x) dm}
      return mempty{modifiedValues=[gi]}
      else if toConstr x == constr (fromJust $ M.lookup gi dm) then do
        us <- MS.evalStateT (sequence $ gmapQ
                  (\x' -> do
                    (i':is') <- MS.get
                    MS.put is'
                    MS.lift $ update' (gRef i') x') x)
          (childRefs $ fromJust $ M.lookup (gRef i) dm)
        return $ mconcat us
        else do
          ds <- remove' gi
          is <- inserti' i x
          return  Updates{ newValues = is
                         , modifiedValues = [gi]
                         , deletedValues = tail ds }

remove :: Ref a -> DMap -> ([GRef],DMap)
remove i = MS.runState (remove' i)

remove' :: Ref a -> MS.State DMap [GRef]
remove' i = do
  m@DMap{dynMap=dm} <- MS.get
  let
    gi = gRef i
    dv = fromJust $ M.lookup gi dm
  MS.put m{dynMap = M.delete gi dm}
  case dv of
    Primitive{} -> return [gi]
    Complex{childRefs=cs} -> do
      ds <- mapM remove' cs
      return $ gi : concat ds

lookup :: Data a => Ref a -> DMap -> a
lookup i dm@DMap{dynMap = m, stopRule = sr} = case M.lookup (gRef i) m of
  Just (Primitive d) -> fromJust $ fromDynamic d
  Just (Complex con cs) -> MS.evalState (fromConstrM f con) cs
    where
      f :: (Data a) => MS.State [GRef] a
      f = do 
            (x:xs) <- MS.get
            MS.put xs
            return $ lookup (gRef x) dm
  Nothing -> error "not found"

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
test3@(us2,dm3) = remove fooRef dm2
