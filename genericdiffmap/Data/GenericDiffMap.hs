{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE FlexibleContexts #-}
module Data.GenericDiffMap 
  ( module Data.GenericDiffMap.Core
  , debugLookup
  , debugUpdate
  , debugInsert
  , debugDelete
  , defaultStopRule
  , jsonStopRule
  , newGDMap2
  , newDynmicGDMap
  , newJSONGDMap)
  where

import Prelude hiding (lookup)
import Text.JSON.Generic
import Data.Data
import Data.Generics.Text
import Data.Dynamic

import Data.Maybe
import Control.Monad.Error 

import Data.GenericDiffMap.Core

instance Data a => Show (GDMapValue a) where
  show (GDPrimitive x) = "GDPrimitive " ++ gshow x
  show (GDComplex cname cs) = "GDComplex " ++ cname ++ " " ++ show cs


debugLookup :: LookupHandler IO
debugLookup = \ref val gdv -> do
  putStrLn $ show ref ++ " lookup: " ++ gshow val ++ " " ++ show gdv
  return val

debugUpdate :: UpdateHandler IO
debugUpdate = \ref old new gdv -> putStrLn $ show ref ++ " updated: " ++ gshow old ++ " -> " ++  gshow new ++ " " ++ show gdv

debugInsert :: InsertHandler IO
debugInsert = \ref inserted gdv -> putStrLn $ show ref ++ " inserted: " ++ gshow inserted ++ " " ++ show gdv

debugDelete :: DeleteHandler IO
debugDelete = \gref -> putStrLn $ show gref ++ " deleted"

defaultStopRule :: forall a . Data a => a -> Bool
defaultStopRule = const False

jsonStopRule :: forall a . Data a => a -> Bool
jsonStopRule x = not (isAlgType dtx) || (tx == typeOf "") || (tx == typeOf True)
  where
    dtx = dataTypeOf x
    tx = typeOf x

newGDMap2 :: (forall a. Data a => a -> Bool) -> (forall a. Data a => a -> b) -> (forall a. Data a => b -> Maybe a) -> GDMap b b
newGDMap2 stopRule to from = newGDMap stopRule to from to from

newDynmicGDMap :: GDMap Dynamic Dynamic
newDynmicGDMap = newGDMap2 defaultStopRule toDyn fromDynamic

-- uses Dynmics for persistence and JSON for serialisation
newJSONGDMap :: GDMap JSValue JSValue
newJSONGDMap = newGDMap jsonStopRule toJSON fromJSON' toJSON fromJSON'
  where
    fromJSON' x = case fromJSON x of
                    Ok x -> Just x
                    Error _ -> Nothing
