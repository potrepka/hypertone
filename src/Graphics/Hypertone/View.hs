module Graphics.Hypertone.View where

import           Control.Concurrent.MVar
import qualified Data.Set                  as Set
import           Graphics.Gloss
import           Graphics.Hypertone.Model
import           Sound.Hypertone.NodeTree
import           Sound.Hypertone.Variables

framesPerSecond :: Int
framesPerSecond = 100

backgroundColor :: Color
backgroundColor = white

window :: Display
window = InWindow "hypertone 0.1.0" (1280, 720) (0, 0)

render :: Client -> IO Picture
render client = do
  let p = 100
  let v = vars client
  controls <- Set.toList <$> readMVar (controlBusses v)
  audios <- Set.toList <$> readMVar (audioBusses v)
  controlPicture <- showControls "control bus" controls
  audioPicture <- showAudios "audio bus" audios
  let scaleDown = scale (1/10) (1/10)
  return $ scale 1 1 $ pictures
    [ pictures $ map Circle [400,410..600]
    , rectangleWire 600 600
    , translate (-300) 200 (scaleDown $ pictures controlPicture)
    , translate 100 200 (scaleDown $ pictures audioPicture) ]

showControl :: (Int, Int) -> Double -> Picture
showControl (x, y) value =
  Text $ "control " ++ show (take y [x..]) ++ " " ++ show value

showControls :: String -> [(Int, Int)] -> IO [Picture]
showControls title list =
  mapM (\(d, i) -> do
         value <- controlGet (fst d)
         return $ translate 0 ((-i) * 200) $ showControl d value)
       (zip list [0..])

showAudio :: (Int, Int) -> Picture
showAudio (x, y) = Text $ "audio " ++ show (take y [x..])

showAudios :: String -> [(Int, Int)] -> IO [Picture]
showAudios title list =
  mapM (\(d, i) ->
         return $ translate 0 (i * 200) $ showAudio d)
       (zip list [0..])
