module Sound.Hypertone where

import Data.Default (def)
import Sound.Hypertone.Live
import Sound.Hypertone.SuperCollider
import Sound.Hypertone.UGen
import Sound.Hypertone.Variables
import qualified Sound.OSC.FD (Transport)
import Sound.OSC
import Sound.SC3
import Sound.SC3.Server.Process

bootServer :: Response -> IO ()
bootServer client = bootSC3 $ onReady openSession client
