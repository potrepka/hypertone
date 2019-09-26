module Sound.Hypertone.Constants where

import           Sound.SC3
import           Sound.SC3.Server.Process.Options
import           Sound.Tidal.Pattern              (Pattern)
import           Sound.Tidal.Stream               (ParamPattern, Shape)

-- NODES

data Bus = ControlBus Int | AudioBus Int deriving (Eq, Ord, Show)
data Node = Bus Bus | Synth Int | Group Int deriving (Eq, Ord, Show)
type Mapping = (Node, Node)

-- STREAMS

type Player = Pattern Double -> ParamPattern
type Router = ParamPattern -> IO ()
type Streamer = Pattern Double -> IO ()

-- MIDI

data MessageKey = CC Int Int
                  | NoteOn Int
                  | NoteOff Int
                  | PolyAftertouch Int
                  | ProgramChange Int
                  | Aftertouch Int
                  | PitchWheel Int
                  | SRTClock
                  | SRTStart deriving (Eq, Ord, Show)

-- DEFAULTS

defaultGroup :: Int
defaultGroup = 1

previewGroup :: Int
previewGroup = 2

busArg :: String
busArg = "bus"

maxDelay :: UGen
maxDelay = 4

-- MATH

tau :: Floating a => a
tau = 2 * pi

goldenRatio :: Floating a => a
goldenRatio = 1.61803398875

-- SERVER OPTIONS

options :: ServerOptions
options = defaultServerOptions

inputs :: Int
inputs = numberOfInputBusChannels options

outputs :: Int
outputs = numberOfOutputBusChannels options

maxAudioBusses :: Int
maxAudioBusses = numberOfAudioBusChannels options

maxControlBusses :: Int
maxControlBusses = numberOfControlBusChannels options

maxNodes :: Int
maxNodes = maxNumberOfNodes options

buffers :: Int
buffers = numberOfSampleBuffers options
