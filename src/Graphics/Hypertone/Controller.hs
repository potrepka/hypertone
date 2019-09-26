module Graphics.Hypertone.Controller where

import           Control.Concurrent.MVar
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Graphics.Hypertone.Model
import           Sound.Hypertone
import           Sound.Hypertone.MIDI
import           Sound.Hypertone.SuperCollider
import           Sound.Hypertone.UGen
import           Sound.Hypertone.Variables
import           System.Exit

-- HANDLE KEY EVENTS
-- KeyLeft/KeyRight, KeySpace
handle :: Event -> Client -> IO Client
handle (EventKey (SpecialKey KeyEsc) Down _ _) client = do
  closeMIDI $ vars client
  quitSC3
  exitSuccess
  return client
handle _ client = return client

-- UPDATE EVERY FRAME
update :: Float -> Client -> IO Client
update seconds client = return client { frames = frames', time = time' }
  where
    frames' = frames client + 1
    time' = time client + seconds
