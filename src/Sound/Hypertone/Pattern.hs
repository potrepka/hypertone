module Sound.Hypertone.Pattern where

import           Control.Concurrent.MVar
import qualified Data.Set                  as Set
import           Sound.Hypertone.Variables
import           Sound.Tidal.Bjorklund
import           Sound.Tidal.Pattern
import           Sound.Tidal.Time

addPattern :: Variables -> Pattern Double -> IO ()
addPattern v p = modifyMVar_ (patterns v) $ \x -> return $ Set.insert p x

removePattern :: Variables -> Pattern Double -> IO ()
removePattern v p = modifyMVar_ (patterns v) $ \x -> return $ Set.delete p x

-- UTILITY



-- RHYTHM

rotate :: [a] -> [a]
rotate p = tail p ++ [head p]

durations :: [Bool] -> [Bool] -> [Rational] -> [Rational]
durations [] y z = tail z ++ [toRational(length y + 1)]
durations (True : xs) y z =
  durations xs [] (z ++ [toRational(length y + 1)])
durations (False : xs) y z = durations xs (True : y) z

euclid :: Int -> Int -> [Rational]
euclid n k = map (/ toRational n)
                 (durations (bjorklund (k, n)) [] [])

euclidPlus :: Int -> Int -> Int -> [Rational]
euclidPlus n k offset = iterate rotate (euclid n k) !! offset

-- PITCH



-- PATTERN BUILDERS

-- repeat a value
repeater :: Time -> a -> Pattern a
repeater n x = _density n (pure x)

-- loop over a list of values
looper :: [a] -> Pattern a
looper = listToPat

-- map a list of durations to a list of values
mapper :: [Time] -> [a] -> Pattern a
mapper durations values = stack $
  let times = scanl (+) 0 durations
      size = last times
  in map (mover size) (zip3 durations (init times) values)
  where
    mover s (x, y, z) = rotR y $ densityGap (s / x)
                               $ _density (1 / s) (pure z)
