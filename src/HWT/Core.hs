module HWT.Core where

import HWT.Types
import qualified Data.GenericDiffMap as GDM

import Control.Monad.State
import Data.Data
import Text.JSON.Generic
import Text.JSON.Pretty

type HWT = State HWTInit

data ConstructorCall = ConstructorCall { referenceName :: String
                                       , constructorCall :: String }

instance Show ConstructorCall where
  show (ConstructorCall ref impl) = "var " ++ ref ++ " = " ++ impl ++ ";"

data HWTInit = HWTInit { nextId :: Int
                       , constructorCalls :: [ConstructorCall]
                       , initialServerValues :: SessionMap
                       , initialClientValues :: SessionMap} deriving Show

newHWTInit :: HWTInit
newHWTInit = HWTInit 0 [] GDM.newJSONGDMap GDM.newJSONGDMap

renderJSON :: JSValue -> String
renderJSON = render . pp_value

newHWTId :: HWT (HWTId a)
newHWTId = do
  i <- gets nextId
  modify $ \ini -> ini{nextId = i + 1}
  return $ HWTId i

addConstructorCall :: String -> String -> HWT ()
addConstructorCall refName impl = modify $ \ini@HWTInit{constructorCalls=cc} -> ini{constructorCalls=cc++[cv]}
  where
    cv = ConstructorCall refName impl

clientValue :: Data a => a -> HWT (ClientValue a)
clientValue x = do
  icvs <- gets initialClientValues
  (ref,icvs') <- GDM.handleInsert x (\r _ gdmv -> clientValue' (ref2Value r) gdmv) icvs
  modify $ \ini -> ini{initialClientValues=icvs'}
  return (ref2Value ref)

clientValue' :: Data a => ClientValue a -> GDM.GDMapValue a -> HWT ()
clientValue' i gdv = addConstructorCall refName impl
  where
    refName = show i
    js = renderJSON $ gdv2JSON gdv
    impl = "new ClientValue('" ++ refName ++ "'," ++ js ++ ")"

serverValue :: Data a => a -> HWT (ServerValue a)
serverValue x = do
  icvs <- gets initialServerValues
  (ref,isvs') <- GDM.handleInsert x (\r _ _ -> serverValue' (ref2Value r)) icvs
  modify $ \ini -> ini{initialServerValues=isvs'}
  return (ref2Value ref)

serverValue' :: Data a => ServerValue a -> HWT ()
serverValue' i = addConstructorCall refName impl
  where
    refName = show i
    impl = "new ServerValue('" ++ refName ++ "')"

transientValue :: Data a => a -> HWT (TransientValue a)
transientValue x = do
  i <- newHWTId
  let
    refName = show i
    js = renderJSON $ toJSON x
    impl = "new TransientValue('" ++ refName ++ "'," ++ js ++ ")"
  addConstructorCall refName impl
  return i

model' :: String -> String -> String -> HWT ()
model' refName m v = do
  let
    impl = "new " ++ m ++ "(" ++ v ++ ")"
  addConstructorCall refName impl
  return ()

readModel' :: Data a => String -> HWT (ReadModel a) 
readModel' v = do
  i <- newHWTId
  model' (show i) "ReadModel" v
  return i

readWriteModel' :: Data a => String -> HWT (ReadWriteModel a) 
readWriteModel' v = do
  i <- newHWTId
  model' (show i) "ReadWriteModel" v
  return i

readModelServer :: Data a => ServerValue a -> HWT (ReadModel a)
readModelServer v = readModel' (show v)

readModelClient :: Data a => ClientValue a -> HWT (ReadModel a)
readModelClient v = readModel' (show v)

readModelTransient :: Data a => TransientValue a -> HWT (ReadModel a)
readModelTransient v = readModel' (show v)

readWriteModelClient :: Data a => ClientValue a -> HWT (ReadWriteModel a)
readWriteModelClient v = readWriteModel' (show v)

readWriteModelTransient :: Data a => TransientValue a -> HWT (ReadWriteModel a)
readWriteModelTransient v = readWriteModel' (show v)

writeModelClient :: Data a => ClientValue a -> HWT (WriteModel a)
writeModelClient v = do
  i <- newHWTId
  model' (show i) "WriteModel" (show v)
  return i
