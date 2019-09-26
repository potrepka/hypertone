module Sound.Hypertone.SuperCollider where

import Data.Default (def)
import Sound.Hypertone.Constants
import Sound.Hypertone.MIDI
import Sound.Hypertone.NodeTree
import Sound.Hypertone.Variables
import qualified Sound.OSC.FD (Transport)
import Sound.OSC
import Sound.SC3
import Sound.SC3.Server.Process

bootSC3 :: (NetworkTransport -> IO ()) -> IO ()
bootSC3 = withSynth
  (def { serverProgram = "scsynth" , loadSynthDefs = False })
  (def { networkPort = UDPPort 57110 })
  def

onReady :: Response -> Response -> NetworkTransport -> IO ()
onReady session client fd = do
  v <- variables
  midi <- openMIDI v
  makeDefaultGroup
  makePreviewGroup
  session v
  client v

quitSC3 :: IO Message
quitSC3 = withSC3 $ async quit
