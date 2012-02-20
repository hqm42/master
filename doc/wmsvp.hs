{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.IORef

-- datatypes + typeclasses

type Ref = IORef

data Value a = Value (Ref a) (Ref [Listener])
data Model a b = Model (Ref [Listener]) (Source a) (a -> b)
newtype Slot a = Slot (IO ())
data Label = forall a. Show a => Label (Ref a)

class IsWidget w where
  renderWidget :: w -> IO String
data Widget = forall a. IsWidget a => Widget a

class IsListener l where
  notify :: l -> IO ()
data Listener = forall a. IsListener a => Listener a

class IsSource s a | s -> a where
  getContent :: s -> IO a
  addListener :: IsListener l => s -> l -> IO ()
data Source a = forall b. IsSource b a => Source b

-- Implementation

instance IsWidget Widget where
  renderWidget (Widget w) = renderWidget w

instance IsWidget Label where
  renderWidget (Label r) = do
    content <- readIORef r
    return ("[Label: " ++ show content ++ "]")

instance IsListener (Slot a) where
  notify (Slot doIt) = doIt

instance IsListener (Model a b) where
  notify (Model ls source f) = do
    listeners <- readIORef ls
    mapM_ (\(Listener l) -> notify l) listeners

instance IsSource (Value a) a where
  getContent (Value r ls) = readIORef r
  addListener (Value r ls) listener =
    modifyIORef ls (Listener listener:)

instance IsSource (Model a b) b where
  getContent (Model ls (Source source) f) = do
    c' <- getContent source
    return (f c')
  addListener (Model ls s f) listener = do
    modifyIORef ls (Listener listener:)
    notify listener

instance (IsSource s1 a, IsSource s2 b)
  => IsSource (s1,s2) (a,b) where
    getContent (s1,s2) = do
      c1 <- getContent s1
      c2 <- getContent s2
      return (c1,c2)
    addListener (s1,s2) listener = do
      addListener s1 listener
      addListener s2 listener

-- values

value :: a -> IO (Value a)
value x = do
  ls <- newIORef []
  r <- newIORef x
  return (Value r ls)

setContent :: Value a -> a -> IO ()
setContent (Value r ls) x' = do
  writeIORef r x'
  listeners <- readIORef ls
  mapM_ (\(Listener l) -> notify l) listeners

-- some usefull models

model :: IsSource s a => s -> (a -> b) -> IO (Model a b)
model source f = do
  ls <- newIORef []
  let
    newModel = Model ls (Source source) f
  addListener source newModel
  return newModel

readModel :: IsSource s a => s -> IO (Model a a)
readModel source = model source id

reverseModel :: IsSource s [a] => s -> IO (Model [a] [a])
reverseModel source = model source reverse

binaryModel :: (IsSource s1 a, IsSource s2 b)
         => (a -> b -> c) -> s1 -> s2 -> IO (Model (a,b) c)
binaryModel f s1 s2 = model (s1,s2) (uncurry f)

andModel, orModel, xorModel
  :: (IsSource s1 Bool, IsSource s2 Bool)
  => s1 -> s2 -> IO (Model (Bool,Bool) Bool)

andModel = binaryModel (&&)
orModel = binaryModel (||)
xorModel = binaryModel (\x y -> (x || y) && not (x && y))

notModel :: IsSource s Bool => s -> IO (Model Bool Bool)
notModel s = model s not

pigLatin :: String -> String
pigLatin s = unwords (map pigLatin' (words s))
  where
    pigLatin' w@(f:r) = if any (== f) "aeiouAEIUO"
    then w ++ "ay"
    else r ++ [f] ++ "ay"

pigLatinModel :: IsSource s String
              => s -> IO (Model String String)
pigLatinModel source = model source pigLatin

-- slots

copySlot :: Model a b -> Ref b -> Slot b
copySlot model ref = Slot (do
  content <- getContent model
  writeIORef ref content)


-- basic label widget

label :: (Show b) => Model a b -> IO Widget
label model = do
  textRef <- newIORef undefined
  addListener model (copySlot model textRef)
  return (Widget (Label textRef))

-- tests

debugWidget :: IsWidget w => w -> IO ()
debugWidget widget = renderWidget widget >>= putStrLn

test1 = do
  putStrLn "test1"
  v <- value "Hallo"
  m1 <- readModel v
  m2 <- reverseModel m1
  l1 <- label m1
  l2 <- label m2
  debugWidget l1
  debugWidget l2
  setContent v "Welt!"
  debugWidget l1
  debugWidget l2

test2 = do
  putStrLn "test2"
  v1 <- value True
  v2 <- value True
  m <- andModel v1 v2
  l <- label m
  debugWidget l
  setContent v1 False
  debugWidget l

main = do
  test1
  test2
