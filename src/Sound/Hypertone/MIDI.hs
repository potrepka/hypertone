module Sound.Hypertone.MIDI where

import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.Map                  as Map
import           Data.Maybe
import           Sound.Hypertone.Constants
import           Sound.Hypertone.NodeTree
import           Sound.Hypertone.Variables
import qualified System.MIDI               as MIDI

openMIDI :: Variables -> IO ()
openMIDI vars = do
  putStrLn "Opening MIDI..."
  modifyMVar_ (midiSources vars) $ \_ -> do
    let listener = Just $ callback vars
    ss <- mapM (`MIDI.openSource` listener) =<< MIDI.enumerateSources
    mapM_ MIDI.start ss
    return ss
  modifyMVar_ (midiDestinations vars) $ \_ -> do
    dd <- mapM MIDI.openDestination =<< MIDI.enumerateDestinations
    mapM_ MIDI.start dd
    return dd

closeMIDI :: Variables -> IO ()
closeMIDI vars = do
  putStrLn "Closing MIDI..."
  modifyMVar_ (midiSources vars) $ \ss -> do
    mapM_ MIDI.stop ss
    mapM_ MIDI.close ss
    return []
  modifyMVar_ (midiDestinations vars) $ \dd -> do
    mapM_ MIDI.stop dd
    mapM_ MIDI.close dd
    return []

printMIDI :: IO ()
printMIDI = do
  putStrLn "---MIDI DEVICES---"
  s <- MIDI.enumerateSources
  d <- MIDI.enumerateDestinations
  putStrLn "---INPUT---"
  mapM_ printDevice s
  putStrLn "---OUTPUT---"
  mapM_ printDevice d
  return ()

printDevice :: MIDI.MIDIHasName a => a -> IO ()
printDevice x = do
  name <- MIDI.getName x
  manufacturer <- MIDI.getManufacturer x
  model <- MIDI.getModel x
  putStrLn $ name ++ " (" ++ manufacturer ++ " : " ++ model ++ ")"
  return ()

allocateOne :: Variables -> MessageKey -> IO Int
allocateOne v key =
  modifyMVar (midiPrimary v) $ \m ->
    if Map.member key m
    then return (m, fromJust $ Map.lookup key m)
    else do
      busA <- controlBus v 1
      return (Map.insert key busA m, busA)

allocateTwo :: Variables -> MessageKey -> IO (Int, Int)
allocateTwo v key = do
  busA <- modifyMVar (midiPrimary v) $ \m ->
    if Map.member key m
    then return (m, fromJust $ Map.lookup key m)
    else do
      busA <- controlBus v 1
      return (Map.insert key busA m, busA)
  busB <- modifyMVar (midiSecondary v) $ \m ->
    if Map.member key m
    then return (m, fromJust $ Map.lookup key m)
    else do
      busB <- controlBus v 1
      return (Map.insert key busB m, busB)
  return (busA, busB)

controlMIDI :: Variables -> MessageKey -> IO Int
controlMIDI v key =
  case key of
    CC channel cc ->
      allocateOne v key
    ProgramChange channel ->
      allocateOne v key
    Aftertouch channel ->
      allocateOne v key
    PitchWheel channel ->
      allocateOne v key
    mk -> return (-1)

controlMIDI2 :: Variables -> MessageKey -> IO (Int, Int)
controlMIDI2 v key =
  case key of
    NoteOn channel ->
      allocateTwo v key
    NoteOff channel ->
      allocateTwo v key
    PolyAftertouch channel ->
      allocateTwo v key
    mk -> return (-1, -1)

normalize :: Double -> Double
normalize x = x / 127

normalizeInt :: Int -> Double
normalizeInt = normalize . fromIntegral

handleOne :: Variables -> MessageKey -> Int -> IO ()
handleOne v key valueA = do
  print key
  busA <- Map.lookup key <$> readMVar (midiPrimary v)
  when (isJust busA) $ controlSet (fromJust busA) (normalizeInt valueA)

handleTwo :: Variables -> MessageKey -> Int -> Int -> IO ()
handleTwo v key valueA valueB = do
  print key
  busA <- Map.lookup key <$> readMVar (midiPrimary v)
  busB <- Map.lookup key <$> readMVar (midiSecondary v)
  when (isJust busA) $ controlSet (fromJust busA) (normalizeInt valueA)
  when (isJust busB) $ controlSet (fromJust busB) (normalizeInt valueB)

callback :: Variables -> MIDI.MidiEvent -> IO ()
callback v (MIDI.MidiEvent millis mm) =
  case mm of
    MIDI.MidiMessage channel (MIDI.NoteOn note velocity) ->
      handleTwo v (NoteOn channel) note velocity
    MIDI.MidiMessage channel (MIDI.NoteOff note velocity) ->
      handleTwo v (NoteOff channel) note velocity
    MIDI.MidiMessage channel (MIDI.PolyAftertouch note pressure) ->
      handleTwo v (PolyAftertouch channel) note pressure
    MIDI.MidiMessage channel (MIDI.CC cc value) ->
      handleOne v (CC channel cc) value
    MIDI.MidiMessage channel (MIDI.ProgramChange program) ->
      handleOne v (ProgramChange channel) program
    MIDI.MidiMessage channel (MIDI.Aftertouch pressure) ->
      handleOne v (Aftertouch channel) pressure
    MIDI.MidiMessage channel (MIDI.PitchWheel pitchBend) ->
      handleOne v (PitchWheel channel) pitchBend
    MIDI.SRTClock ->
      handleOne v SRTClock 0 -- TODO make this add 1/24
    MIDI.SRTStart ->
      handleOne v SRTStart 1 -- TODO also make this reset clock
    MIDI.SRTContinue ->
      handleOne v SRTStart 1
    MIDI.SRTStop ->
      handleOne v SRTStart 0
    e ->
      print e

sendMIDI :: Variables -> Int -> MIDI.MidiMessage -> IO ()
sendMIDI v i mm = do
  dd <- readMVar (midiDestinations v)
  MIDI.send (dd !! i) mm
