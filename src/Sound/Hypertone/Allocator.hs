module Sound.Hypertone.Allocator where

import           Control.Concurrent.MVar
import           Data.List

-- new allocator with n bits
newAllocator :: Int -> IO (MVar [Bool])
newAllocator n = newMVar $ replicate n False

-- allocate n bits
-- returns index
allocate :: MVar [Bool] -> Int -> IO (Maybe Int)
allocate aa n =
  modifyMVar aa $ \a ->
    let f = replicate n False
        g = group a
        e = findIndex (isPrefixOf f) g
    in case e of
      Just w -> do
        let (x, y) = splitAt w g
        let xx = concat x
        let yy = concat y
        let zz = xx ++ replicate n True ++ drop n yy
        return (zz, Just $ length xx)
      Nothing -> return (a, Nothing)

-- deallocate n bits at index i
deallocate :: MVar [Bool] -> Int -> Int -> IO ()
deallocate aa i n =
  modifyMVar_ aa $ \a -> do
    let (x, y) = splitAt i a
    return $ x ++ replicate n False ++ drop n y
