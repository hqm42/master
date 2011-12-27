{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GenericDiffMap.Tests where

import Data.Data
import Control.Monad ((>=>))
import Prelude hiding (lookup)

import Data.GenericDiffMap

data User = U { name :: String
              , age :: Int
              , address :: Address} deriving (Show, Typeable, Data)

data Address = A { city :: String
                 , street :: String
                 , houseNumber :: Int} deriving (Show, Typeable, Data)

$(derivePs ''User) -- magic
$(derivePs ''Address)


robert = U "Robert" 26 $ A "Hamburg" "Reeperbahn" 42

test1 = do
  (rob,m1) <- handleInsert robert debugInsert newJSONGDMap
  let
    rob_add = projection address_p rob m1
  m2 <- handleUpdate rob_add (A "Berlin" "Kudamm" 123) debugUpdate debugInsert debugDelete m1
  putStrLn $ show $ lookup rob m2
  let
    rob_add_hn = projection (address_p >=> houseNumber_p) rob m2
  handleUpdate rob_add_hn 815 debugUpdate debugInsert debugDelete m2
