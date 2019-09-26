module Sound.Hypertone.Live where

import           Control.Concurrent.MVar
import           Sound.Hypertone.Constants
import           Sound.Hypertone.MIDI
import           Sound.Hypertone.NodeTree
import           Sound.Hypertone.Pattern
import           Sound.Hypertone.Stream
import           Sound.Hypertone.Synth
import           Sound.Hypertone.UGen
import           Sound.Hypertone.Variables
import           Sound.Tidal.Stream        (Param (F), makeF)

openSession :: Variables -> IO ()
openSession v = do
  putStrLn "Opening session..."

  -- BUSSES
  let hardwareIn = hardwareInBus v
  let hardwareOut = hardwareOutBus v

  cBus <- controlBus v 1

  cBus1 <- controlMIDI v (CC 1 77)
  cBus2 <- controlMIDI v (CC 1 93)
  cBus3 <- controlMIDI v (CC 1 73)
  cBus4 <- controlMIDI v (CC 1 75)

  cBus5 <- controlMIDI v (CC 1 17)
  cBus6 <- controlMIDI v (CC 1 91)
  cBus7 <- controlMIDI v (CC 1 79)
  cBus8 <- controlMIDI v (CC 1 72)

  aBus <- audioBus v 2
  --controlSet cBus 68

  -- SYNTHS
  preview1 <- publish v "pitcher" $ outBus (fromIntegral cBus) pitchOsc
  preview2 <- publish v "quad" $ outBus hardwareOut quadFreq

  -- NODES
  n <- start v defaultGroup "quad" []
  xoxo <- start v defaultGroup "pitcher" []

  -- CONTROL INPUT BUS CONNECTIONS
  controlIn v cBus n "pitchL"
  controlIn v cBus n "pitchR"
  controlIn v cBus n "tripleL"
  controlIn v cBus n "tripleR"

  controlIn v cBus1 n "doubleL"
  controlIn v cBus2 n "attackL"
  controlIn v cBus3 n "releaseL"
  controlIn v cBus4 n "verbL"

  controlIn v cBus5 n "doubleR"
  controlIn v cBus6 n "attackR"
  controlIn v cBus7 n "releaseR"
  controlIn v cBus8 n "verbR"

  -- AUDIO INPUT BUS CONNECTIONS

  -- OUTPUT BUS CONNECTIONS
  setBus v n hardwareOut
  setBus v xoxo cBus

  -- PATTERN BANK
  let rr = repeater 6 1.0
  let mm = mapper (euclid 8 3)
                  (replicate 3 1.0)
  --let ll = looper [1 0 1 0]
  print mm

  -- STREAMS
  gate <- streamer v n "gate" 0

  gate mm

  --print =<< controlGet cBus

  -- ALWAYS DO THIS
  sortNodes v

  print =<< readMVar (nodeMappings v)
  print =<< readMVar (nodeArgToBus v)

  return ()
