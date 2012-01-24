{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.GenericDiffMap.Projection where

import Data.Data
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe

import Control.Monad.Writer

data ProjectionStep = PS String Int deriving Show

type Projection a b = a -> Writer [ProjectionStep] b

pa :: (a -> b) -> ProjectionStep -> Projection a b
pa p ps = \x -> do
  tell [ps]
  return $ p x

runP :: Projection a b -> a -> (b,[ProjectionStep])
runP pa a = runWriter (pa a)

execP :: Projection _a _b -> [ProjectionStep]
execP a = snd $ runP a undefined

evalP :: Projection a b -> a -> b
evalP a x = fst $ runP a x

deriveP :: Name -> Name -> Name -> Int -> DecQ
deriveP oldName newName constr index = do
  let
    funName = VarP newName
    projectionFunction = VarE oldName
    cname = nameBase constr
  pa <- [|pa|]
  ps <- [|PS cname index|]
  return $ ValD funName (NormalB (AppE (AppE pa projectionFunction) ps)) []

deriveNamedPs :: Name -> (String -> Maybe String) -> Q [Dec]
deriveNamedPs t namegen = do
  info <- reify t
  let
    (tyname, cons) = case info of
      TyConI (DataD    _ n _ cs _) -> (n, cs)
      TyConI (NewtypeD _ n _ c  _) -> (n, [c])
      _                             -> error "Can only derive projections for datatypes and newtypes."
    sels = concat $ map (\(RecC n fs) -> map (\(i,(pn,_,_)) -> (pn,n,i)) (zip [0..] fs)) cons
  mapM (\(n,n',c,i) -> deriveP n n' c i) [(n,n'',c,i)| (n,c,i) <- sels
                                                     , let n' = (namegen $ nameBase n)
                                                     , isJust n'
                                                     , let n'' = mkName $ fromJust n']

derivePs :: Name -> Q [Dec]
derivePs t = deriveNamedPs t (\s -> Just $ s ++ "_p")

-- some usefull projections

head_p :: Projection [a] a
head_p = pa head $ PS "head" 0
tail_p :: Projection [a] [a]
tail_p = pa tail $ PS "tail" 1

fst_p :: Projection (a,b) a
fst_p = pa fst $ PS "fst" 0
snd_p :: Projection (a,b) b
snd_p = pa snd $ PS "snd" 1

lookup_p :: Eq a => a -> Projection [(a,b)] b
lookup_p key [] = error "lookup_p failed: key not found"
lookup_p key ((key',val):kvs) = if key == key'
  then do
    tell [PS "head" 0, PS "snd" 1]
    return val
  else do
    tell [PS "tail" 1]
    lookup_p key kvs

atIndex_p :: Int -> Projection [a] a
atIndex_p i (x:xs) | i == 0 = do
                                tell [PS "head" 0]
                                return x
                   | i > 0  = do
                                tell [PS "tail" 1]
                                atIndex_p (i-1) xs
                   | i < 0  = error "atIndex_p: negative index"
atIndex_p i [] = error "atIndex_p: empty list"
