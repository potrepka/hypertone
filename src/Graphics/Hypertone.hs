module Graphics.Hypertone where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Hypertone.Controller
import Graphics.Hypertone.Model
import Graphics.Hypertone.View
import Sound.Hypertone.MIDI
import Sound.Hypertone.Variables

openClient :: Variables -> IO ()
openClient v = do
  print "Opening client..."
  c <- client v
  playIO
      window
      backgroundColor
      framesPerSecond
      c
      render
      handle
      update
