module Sound.Hypertone.Synth where

import           Control.Concurrent.MVar
import           Data.Map
import           Sound.Hypertone.NodeTree
import           Sound.Hypertone.UGen
import           Sound.Hypertone.Variables
import           Sound.OSC
import           Sound.SC3

publish :: Variables -> String -> UGen -> IO Int
publish v name u = do
  withSC3 $ async $ d_recv $ synthdef name u
  i <- preview v name
  modifyMVar_ (previewNodes v) $ \m -> return $ insert name i m
  modifyMVar_ (synthDefinitions v) $ \m -> return $ insert name u m
  return i

unpublish :: Variables -> String -> IO ()
unpublish v name = do
  stopAll v name
  withSC3 $ async $ d_free [name]
  modifyMVar_ (previewNodes v) $ \m -> return $ delete name m
  modifyMVar_ (synthDefinitions v) $ \m -> return $ delete name m

-- SYNTHS


-- TESTING

pitchOsc :: UGen
pitchOsc = controlSine 1 0 * 24 + 61

quadFreq :: UGen
quadFreq =
  let p3 = pitchToFreq $ control2 "pitch" 0
      p2 = toFreq $ control2 "double" 0
      p1 = pitchToFreq $ control2 "triple" 0
      rv = control2 "verb" 0
      at = control2 "attack" 0.1
      re = control2 "release" 0.1
      gate = tr_control "gate" 0
      s1 = sine p1 0
      s2 = sine (12 + p1) 0
      x1 = sine p2 s1
      x2 = sine (12 + p2) s2
      y1 = sine p3 x1
      y2 = sine (12 + p3) x2
      signal = parallel y1 y2 0.5
      fv = reverb2 signal 1.0 0.0
      s = envPerc at re
      e = envGen KR gate 1 0 1 DoNothing s
  in parallel signal fv rv * e
