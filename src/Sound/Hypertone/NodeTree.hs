module Sound.Hypertone.NodeTree where

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Graph
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import qualified Data.MultiMap             as MultiMap
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Tuple
import           Sound.Hypertone.Allocator
import           Sound.Hypertone.Constants
import           Sound.Hypertone.Variables
import           Sound.OSC
import           Sound.SC3

-- NODE

group :: Variables -> Int -> IO Int
group v parent = do
  n <- allocate (nodeAllocator v) 1
  let node = 1000 + fromJust n
  putStrLn $ "starting group " ++ show node
  withSC3 $ sendMessage $ g_new [(node, AddToTail, parent)]
  modifyMVar_ (groupNodes v) $ \s -> return $ Set.insert node s
  return node

start :: Variables -> Int -> String -> [(String, Double)] -> IO Int
start v parent name controls = do
  n <- allocate (nodeAllocator v) 1
  let node = 1000 + fromJust n
  putStrLn $ "starting node " ++ show node
  withSC3 $ sendMessage $ s_new name node AddToTail parent controls
  modifyMVar_ (synthNodes v) $ \s -> return $ Set.insert (name, node) s
  return node

-- set node arg to value
set :: Variables -> Int -> String -> Double -> IO ()
set v node arg value =
  modifyMVar_ (nodeArgToBus v) $ \ab -> do
    withSC3 $ sendMessage $ n_set node [(arg, value)]
    --look up ugen rate
    rate <- rateOf <$> getUGen v node
    when (arg == "bus") $ modifyMVar_ (nodeMappings v) $ \x ->
      return $ Set.insert (Synth node, Bus (if rate == KR
                                            then ControlBus (round value)
                                            else AudioBus (round value)))
             $ Set.filter (\y -> Synth node /= fst y) x
    return $ Map.delete (node, arg) ab

stop :: Variables -> Int -> IO ()
stop v node =
  modifyMVar_ (synthNodes v) $ \s -> do
    putStrLn $ "stopping node " ++ show node
    deallocate (nodeAllocator v) node 1
    withSC3 $ sendMessage $ n_free [node]
    return $ Set.filter (\x -> node /= snd x) s

stopAll :: Variables -> String -> IO ()
stopAll v name =
  modifyMVar_ (synthNodes v) $ \s -> do
    putStrLn $ "stopping all nodes for " ++ name
    let (yes, no) = Set.partition (\x -> name == fst x) s
    let nodes = map snd $ Set.toList yes
    mapM_ (\n -> deallocate (nodeAllocator v) n 1) nodes
    withSC3 $ sendMessage $ n_free nodes
    return no

getUGen :: Variables -> Int -> IO UGen
getUGen v node = do
  s <- readMVar (synthNodes v)
  sd <- readMVar (synthDefinitions v)
  return $ fromJust
         $ Map.lookup (fst $ head
                           $ Set.toList
                           $ Set.filter (\x -> node == snd x) s) sd

-- BUS

controlBus :: Variables -> Int -> IO Int
controlBus v channels = do
  i <- fromJust <$> allocate (controlAllocator v) channels
  modifyMVar_ (controlBusses v) $ \x -> return $ Set.insert (i, channels) x
  return i

audioBus :: Variables -> Int -> IO Int
audioBus v channels = do
  i <- fromJust <$> allocate (audioAllocator v) channels
  modifyMVar_ (audioBusses v) $ \x -> return $ Set.insert (i, channels) x
  return i

free :: Variables -> Bus -> Int -> IO ()
free v bus channels =
  case bus of
    ControlBus i -> do
      deallocate (controlAllocator v) i channels
      modifyMVar_ (controlBusses v) $ \x -> return $ Set.delete (i, channels) x
    AudioBus i   -> do
      deallocate (audioAllocator v) i channels
      modifyMVar_ (audioBusses v) $ \x -> return $ Set.delete (i, channels) x

-- get bus value
controlGet :: Int -> IO Double
controlGet index =
  withSC3 $ fmap (fromJust . datum_floating . last . messageDatum)
                 (sendMessage (c_get [index]) >> waitReply "/c_set")

-- set bus value
controlSet :: Int -> Double -> IO ()
controlSet index value = withSC3 $ sendMessage $ c_set [(index, value)]

-- apply function to bus value
controlApply :: (Double -> Double) -> Int -> IO ()
controlApply f index = controlSet index =<< f <$> controlGet index

-- connect bus to node arg
controlIn :: Variables -> Int -> Int -> String -> IO ()
controlIn v index node arg =
  modifyMVar_ (nodeArgToBus v) $ \ab -> do
    withSC3 $ sendMessage $ n_map node [(arg, index)]
    return $ Map.insert (node, arg) (ControlBus index)
           $ Map.delete (node, arg) ab

-- connect bus to node arg
audioIn :: Variables -> Int -> Int -> String -> IO ()
audioIn v index node arg =
  modifyMVar_ (nodeArgToBus v) $ \ab -> do
    withSC3 $ sendMessage $ n_mapa node [(arg, index)]
    return $ Map.insert (node, arg) (AudioBus index)
           $ Map.delete (node, arg) ab

-- connect node to bus
setBus :: Variables -> Int -> Int -> IO ()
setBus v node index = set v node busArg (fromIntegral index)

-- TREE

makeDefaultGroup :: IO ()
makeDefaultGroup = withSC3 $ sendMessage $ g_new [(defaultGroup, AddToTail, 0)]

makePreviewGroup :: IO ()
makePreviewGroup = withSC3 $ sendMessage $ g_new [(previewGroup, AddToTail, 0)]

preview :: Variables -> String -> IO Int
preview v name =
  start v previewGroup name [(busArg, fromIntegral $ hardwareOutBus v)]

nodeArgToBusToNodeMappings :: Map (Int, String) Bus -> Set Mapping
nodeArgToBusToNodeMappings ab =
  Set.fromList $ map (\(a, b) -> (Bus b, Synth $ fst a)) $ Map.toList ab

sortNodes :: Variables -> IO ()
sortNodes v = do
  nm <- readMVar (nodeMappings v)
  ab <- readMVar (nodeArgToBus v)
  --sidenote: mappings is sorted
  let mappings = Set.toList (nodeArgToBusToNodeMappings ab) ++ Set.toList nm
  --create a graph
  let (graph, vertexToNode, _) = graphFromEdges $ map (\(x, y) -> (x, x, y))
                                                $ MultiMap.assocs
                                                $ MultiMap.fromList mappings
  --perform a topological sort
  --send a message to order the nodes
  withSC3 $ sendMessage
          $ n_order AddToHead defaultGroup
          $ map (\(_, Synth i, _) -> i)
          $ filter (\(_, x, _) -> case x of Bus _   -> False
                                            Synth _ -> True
                                            Group _ -> False)
          $ map vertexToNode
          $ topSort graph
  print $ map vertexToNode $ topSort graph
  return ()
