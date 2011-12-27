{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.GenericDiffMap.Projection where

import Data.Data
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe

import Control.Monad.Writer

data ProjectionStep = PS String Int deriving Show

type Projection a b = a -> Writer [ProjectionStep] b

pa :: Data a => (a -> b) -> ProjectionStep -> Projection a b
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
  mapM (\(n,n',c,i) -> deriveP n n' c i) [(n,n'',c,i)| (n,c,i) <- sels, let n' = (namegen $ nameBase n), isJust n', let n'' = mkName $ fromJust n']

derivePs :: Name -> Q [Dec]
derivePs t = deriveNamedPs t (\s -> Just $ s ++ "_p")
