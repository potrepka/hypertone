module Sound.Hypertone.Stream where

import           Control.Concurrent.MVar
import qualified Data.Map                  as Map
import           Sound.Hypertone.Constants
import           Sound.Hypertone.Variables
import           Sound.OSC.Datum
import           Sound.Tidal.Context
import           Sound.Tidal.OscStream
import           Sound.Tidal.Pattern
import           Sound.Tidal.Stream

nodeSet :: Int -> OscSlang
nodeSet node = OscSlang {
  path = "/n_set",
  preamble = [int32 node],
  namedParams = True,
  timestamp = BundleStamp
}

nodeBackend :: Int -> IO (Backend a)
nodeBackend node = do
  s <- makeConnection "127.0.0.1" 57110 (nodeSet node)
  return $ Backend s (\_ _ _ -> return ())

nodeStream :: Int -> [Param] -> Double -> IO (Router, Shape)
nodeStream node ps l = do
  let shape = supercollider ps l
  backend <- nodeBackend node
  router <- stream backend shape
  return (router, shape)

streamer :: Variables -> Int -> String -> Double -> IO Streamer
streamer v node arg defaultValue = do
  latency <- readMVar $ globalLatency v
  (router, shape) <- nodeStream node [ F arg (Just defaultValue) ] latency
  let player = makeF shape arg
  let s = router . player
  modifyMVar_ (nodeStreamers v) $ \ns -> return $ Map.insert (node, arg) s ns
  return s
