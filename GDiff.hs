{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module GDiff where

import Prelude hiding (lookup)
import Text.JSON.Generic
import Data.Data
import Data.Dynamic
import qualified Data.Map as M
import qualified Control.Monad.State as MS
import Data.Maybe (fromJust)
import Data.Monoid

data Foo1 = Foo1 {
  bar1 :: Int,
  baz1 :: [String]
} deriving (Eq,Show,Typeable,Data)

foo = Foo1 42 ["Hallo","Welt"]


data Foo3 = Foo31 Int 
          | Foo32 Int
          deriving (Eq,Show,Typeable,Data)

data DestructuredValue = Primitive {
  primitive :: Dynamic
} | Complex  {
  constr :: Constr,
  childRefs :: [Int]
} deriving Show

data DMap = DMap {
  nextId :: Int,
  dynMap :: M.Map Int DestructuredValue,
  stopRule :: forall a. Data a => a -> Bool
}

data Updates = Updates {
  newValues :: [Int],
  modifiedValues :: [Int],
  deletedValues :: [Int]
}

instance Monoid Updates where
  mempty = Updates [] [] []
  mappend (Updates n m d) (Updates n' m' d') = Updates (n ++ n') (m ++ m') (d ++ d')

instance Show DMap where
  show DMap{dynMap=dm} = "DMap " ++ show dm

insert :: Data a => a -> DMap -> (Int,DMap)
insert x m = MS.runState (insert' x) m

insert' :: Data a => a -> MS.State DMap Int
insert' x = do
  stopRule <- MS.gets stopRule
  if stopRule x
    then do
        thisId <- MS.gets nextId
        MS.modify $ \dm -> dm{ nextId = thisId + 1
                             , dynMap = M.insert thisId (Primitive $ toDyn x) (dynMap dm)}
        return thisId
    else do
        children <- sequence $ gmapQ insert' x
        thisId <- MS.gets nextId
        MS.modify $ \dm -> dm{ nextId = thisId + 1
                             , dynMap = M.insert thisId (Complex (toConstr x) children) (dynMap dm)}
        return thisId

-- replaces the current value with a new one. fails if type does not match, or value not present
update :: (Eq a, Data a) => Int -> a -> DMap -> (Updates,DMap)
update i x m = MS.runState (update' i x) m

gmapQ' :: (forall d. (Data d,Eq d) => d -> u) -> (forall a. Data a => a) -> [u]
gmapQ' f x = gmapQ f x

update' :: (Eq a, Data a) => Int -> a -> MS.State DMap Updates
update' i x = do
  stopRule <- MS.gets stopRule
  dm <- MS.gets dynMap
  let
    thisOld = fromJust $ M.lookup i dm
  if stopRule x
    then if x == (fromJust $ fromDynamic $ primitive thisOld)
      then return mempty
      else do
        MS.modify $ \m -> m{dynMap=M.insert i (Primitive $ toDyn x) dm}
        return mempty{modifiedValues=[i]}
    else if toConstr x == (constr thisOld) 
      then do
        updates <- sequence $ gmapQ (update' i) x
        return $ mconcat updates
      else
        undefined

lookup :: Data a => Int -> DMap -> a
lookup i dm@DMap{dynMap = m, stopRule = sr} = case M.lookup i m of
  Just (Primitive d) -> fromJust $ fromDynamic d
  Just (Complex con cs) -> MS.evalState (fromConstrM f con) cs
    where
      f :: (Data a) => MS.State [Int] a
      f = do 
            (x:xs) <- MS.get
            MS.put xs
            return $ lookup x dm
  Nothing -> error "not found"

defaultStopRule :: forall a . Data a => a -> Bool
defaultStopRule = isAlgType . dataTypeOf

betterStopRule :: forall a . Data a => a -> Bool
betterStopRule x = not (isAlgType dtx) || (tx == typeOf "")
  where
    cx = toConstr x
    dtx = dataTypeOf x
    tx = typeOf x

empty = DMap{ nextId = 0
            , dynMap = M.empty
            , stopRule = betterStopRule}
