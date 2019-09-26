module Sound.Hypertone.Variables where

import           Control.Concurrent.MVar   (MVar, newMVar, readMVar)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust)
import           Data.MultiMap             (MultiMap)
import qualified Data.MultiMap             as MultiMap
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Sound.Hypertone.Allocator
import           Sound.Hypertone.Constants
import           Sound.SC3.UGen.Type       (UGen)
import           Sound.Tidal.Pattern       (Pattern)
import           Sound.Tidal.Stream        (ParamPattern, Shape)
import           System.IO
import           System.MIDI               (Connection, MidiMessage)

data Variables = Variables { globalLatency :: MVar Double
                           , controlAllocator :: MVar [Bool]
                           , controlBusses :: MVar (Set (Int, Int))
                           , audioAllocator :: MVar [Bool]
                           , audioBusses :: MVar (Set (Int, Int))
                           , hardwareOutBus :: Int
                           , hardwareInBus :: Int
                           , nodeAllocator :: MVar [Bool]
                           , groupNodes :: MVar (Set Int)
                           , synthNodes :: MVar (Set (String, Int))
                           , previewNodes :: MVar (Map String Int)
                           , synthDefinitions :: MVar (Map String UGen)
                           , nodeStreamers :: MVar (Map (Int, String) Streamer)
                           , nodeMappings :: MVar (Set Mapping)
                           , nodeArgToBus :: MVar (Map (Int, String) Bus)
                           , midiSources :: MVar [Connection]
                           , midiDestinations :: MVar [Connection]
                           , midiPrimary :: MVar (Map MessageKey Int)
                           , midiSecondary :: MVar (Map MessageKey Int)
                           , tempoBus :: Int
                           , patterns :: MVar (Set (Pattern Double)) }

type Response = Variables -> IO ()

variables :: IO Variables
variables = do
  l <- newMVar 0.060
  c <- newAllocator maxControlBusses
  cc <- newMVar Set.empty
  a <- newAllocator maxAudioBusses
  aa <- newMVar Set.empty
  o <- allocate a outputs
  i <- allocate a inputs
  n <- newAllocator maxNodes
  g <- newMVar Set.empty
  s <- newMVar Set.empty
  p <- newMVar Map.empty
  sd <- newMVar Map.empty
  ns <- newMVar Map.empty
  nm <- newMVar Set.empty
  ab <- newMVar Map.empty
  ms <- newMVar []
  md <- newMVar []
  m1 <- newMVar Map.empty
  m2 <- newMVar Map.empty
  t <- allocate c 1 --cycles per second
  pa <- newMVar Set.empty
  return Variables { globalLatency = l
                   , controlAllocator = c
                   , controlBusses = cc
                   , audioAllocator = a
                   , audioBusses = aa
                   , hardwareOutBus = fromJust o
                   , hardwareInBus = fromJust i
                   , nodeAllocator = n
                   , groupNodes = g
                   , synthNodes = s
                   , previewNodes = p
                   , synthDefinitions = sd
                   , nodeStreamers = ns
                   , nodeMappings = nm
                   , nodeArgToBus = ab
                   , midiSources = ms
                   , midiDestinations = md
                   , midiPrimary = m1
                   , midiSecondary = m2
                   , tempoBus = fromJust t
                   , patterns = pa }

-- TODO ... save and load

loadVariables :: String -> IO ()
loadVariables filename = do
  contents <- readFile filename
  putStr "demo: "
  putStrLn contents

saveVariables :: String -> IO ()
saveVariables filename =
  writeFile filename "no file"
