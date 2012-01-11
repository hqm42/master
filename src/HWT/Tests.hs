module HWT.Tests where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer hiding (listen)
import Control.Concurrent.AdvSTM
import Control.Concurrent.AdvSTM.TVar
import Control.Concurrent.AdvSTM.TMVar
import Data.Monoid
import Data.Maybe
import qualified Data.IntMap as IM

import Data.GenericDiffMap.Tests hiding (test1)
import HWT.Types
import HWT.Core
import HWT.Session

test1 = runState (do
  rob <- clientValue robert
  rm_rob <- readModel rob
  rm_rob_name <- projection name_p rm_rob
  label rm_rob_name) newHWTInit

-- type HWTAction = WriterT SessionUpdates (ReaderT HWTSession AdvSTM)

test2 :: IO ()
test2 = do
  let
    (l,ini) = runState (do
      rob <- clientValue robert
      rm_rob <- readModel rob
      rm_rob_name <- projection name_p rm_rob
      vl <- valueListener rob (do
                                 r <- getValue rob
                                 setValue rob r{name="Roberto"}
                                 return ())
      label rm_rob_name) newHWTInit
  putStrLn $ show ini
  putStrLn "-------------------------"
  let
    listener = head $ fromJust $ IM.lookup 0 $ initialClientValueListeners $ ini
  svsT <- newTVarIO (initialServerValues ini)
  cvsT <- newTVarIO (initialClientValues ini)
  wvsT <- newTVarIO (initialWindowValues ini)
  upsT <- newTMVarIO mempty
  sessionsT <- newTVarIO []
  let
    session = HWTSession svsT cvsT wvsT upsT sessionsT
  res <- atomically $ runReaderT (runWriterT (listen listener)) session
  putStrLn $ show res
