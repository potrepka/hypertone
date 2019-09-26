module Graphics.Hypertone.Model where

import Sound.Hypertone.Variables
import qualified System.MIDI as MIDI

data Client = Client { frames :: Int
                     , time :: Float
                     , vars :: Variables }

client :: Variables -> IO Client
client v = return Client { frames = 0
                         , time = 0
                         , vars = v }
