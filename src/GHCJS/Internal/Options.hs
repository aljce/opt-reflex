module GHCJS.Internal.Options (mainWith) where

import Control.Concurrent     (forkIO)
import Control.Concurrent.STM (atomically,newEmptyTMVarIO,takeTMVar,TMVar)

import Control.Monad (void)

mainWith :: (Maybe a -> IO b) -> (TMVar a -> IO ()) -> Bool -> IO b
mainWith main genOpts = \case
  True -> do
    opts <- newEmptyTMVarIO
    void (forkIO (genOpts opts))
    main . Just =<< atomically (takeTMVar opts)
  False -> main Nothing
