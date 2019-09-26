module Sound.Hypertone.UGen where

import           Sound.Hypertone.Constants
import           Sound.Hypertone.Variables
import           Sound.OSC
import           Sound.SC3

-- * mixer ugens - filters, delay + verb, distortion, compression/expansion
-- ** oscillator ugens - FM (bandlimited), AM + RM (bandlimited), wavetable, sample
-- *** control ugens - envelope (EnvGen), LFO (non-bandlimited), lag (VarLag)

-- ** OSCILLATOR
-- * EFFECTS
-- * filters
-- * dynamics
-- * space
-- *** CONTROLS - envelope lfo lag
-- ** SAMPLER
-- ** WAVETABLES

-- TODO: OFFICIAL LIST


-- finish lag, control mixers, oscillators, and effects
-- compression / expansion
-- noise gate / noise amplification
-- non-linear distortion
-- hard limiter
-- BEQSuite
-- Moog?


-- GENERATE A LIST OF PARAMS



-- GENERATE A UGEN





-- MATH

nyquist :: UGen
nyquist = 0.5 * sampleRate

nyquistMIDI :: UGen
nyquistMIDI = floorE $ cpsMIDI nyquist

-- BUS

inBus :: Rate -> Int -> Int -> UGen
inBus rate bus channels = in' channels rate (fromIntegral bus)

outBus :: Int -> UGen -> UGen
outBus bus = out (control KR busArg (fromIntegral bus))

--tempo :: UGen
--tempo = in' 1 KR tempoBus

{-|}

secondsPerBeat :: UGen
secondsPerBeat = 60 / 120

ratioToDelta :: UGen -> UGen
ratioToDelta u = cpsMIDI u - cpsMIDI 1

deltaToRatio :: UGen -> UGen
deltaToRatio u = midiCPS $ u + cpsMIDI 1

pitchToTime :: UGen -> UGen
pitchToTime u = secondsPerBeat * deltaToRatio u

timeToPitch :: UGen -> UGen
timeToPitch u = ratioToDelta (u / secondsPerBeat)
|-}

-- CONTROLS

param2 :: Rate -> String -> Double -> UGen
param2 rate name value = mce2 (control rate (name ++ "L") value)
                              (control rate (name ++ "R") value)

initial2 :: String -> Double -> UGen
initial2 = param2 IR

control2 :: String -> Double -> UGen
control2 = param2 KR

audio2 :: String -> Double -> UGen
audio2 = param2 AR

trigger :: String -> Double -> UGen
trigger = tr_control

withAmp :: UGen -> UGen -> UGen
withAmp amplitude u = u * amplitude

pitchToFreq :: UGen -> UGen
pitchToFreq = midiCPS

toPitch :: UGen -> UGen
toPitch = (*) 127

toFreq :: UGen -> UGen
toFreq = pitchToFreq . toPitch

toRQ :: UGen -> UGen
toRQ linear = exp (linear * 2 - 1)

toDB :: UGen -> UGen
toDB linear = ampDb (linear * 2)

-- LOW-FREQUENCY OSCILLATORS
-- freq: cycles per second
-- phase: 0-1
-- iphase: 0-1
-- duty: 0-1

controlSine :: UGen -> UGen -> UGen
controlSine freq phase = sinOsc KR freq (phase * tau) * 0.5

controlSaw :: UGen -> UGen -> UGen -> UGen
controlSaw freq iphase duty = varSaw KR freq iphase duty * 0.5

controlPulse :: UGen -> UGen -> UGen -> UGen
controlPulse freq iphase duty = lfPulse KR freq iphase duty - 0.5

noiseStep :: UGen -> UGen
noiseStep freq = lfdNoise0 (0.0 :: Double) KR freq * 0.5

noiseLinear :: UGen -> UGen
noiseLinear freq = lfdNoise1 (0.0 :: Double) KR freq * 0.5

noiseCubic :: UGen -> UGen
noiseCubic freq = lfdNoise3 (0.0 :: Double) KR freq * 0.5

-- CONTROL FUNCTIONS

-- smooth u time
smooth :: UGen -> UGen -> UGen
smooth = lag3

hold :: UGen -> UGen -> UGen
hold = trig

-- ENVELOPES
-- TODO make more envelopes

perc :: UGen -> UGen -> UGen -> UGen
perc trigger attack release =
  envGen KR trigger 1 0 1 DoNothing $ envPerc attack release

-- AUDIO GENERATORS

-- freq: cycles per second
-- phase: 0-1
-- feedback: 0-1
-- iphase: 0-1 (IR)
-- duty: 0-1

sine :: UGen -> UGen -> UGen
sine freq phase = sinOsc AR freq (phase * tau) * 0.5

sineFeedback :: UGen -> UGen -> UGen
sineFeedback freq feedback = sinOscFB AR freq (feedback * goldenRatio) * 0.5

-- band-limitied signal
bandSaw :: UGen -> UGen
bandSaw freq = saw AR freq * 0.5

variableSaw :: UGen -> UGen -> UGen -> UGen
variableSaw freq iphase duty = varSaw AR freq iphase duty * 0.5

-- band-limited signal
bandPulse :: UGen -> UGen -> UGen
bandPulse freq duty = pulse AR freq duty * 0.5

variablePulse :: UGen -> UGen -> UGen -> UGen
variablePulse freq iphase duty = lfPulse KR freq iphase duty - 0.5

white :: UGen
white = whiteNoise (0.0 :: Double) AR

pink :: UGen
pink = pinkNoise (0.0 :: Double) AR

gray :: UGen
gray = grayNoise (0.0 :: Double) AR

-- AUDIO FUNCTIONS

-- u: signal
-- v: secondary signal
-- mix: 0-1

thru :: UGen -> UGen
thru = id

inverse :: UGen -> UGen
inverse u = 1 - u

ringMod :: UGen -> UGen -> UGen
ringMod u v = u * v * 2

ampMod :: UGen -> UGen -> UGen
ampMod u v = u * (v + 0.5)

parallel :: UGen -> UGen -> UGen -> UGen
parallel u v mix =
    let weight = clip mix 0 1
        uu = u * (1 - weight)
        vv = v * weight
    in uu + vv

-- SPACE

-- u: signal
-- pos: 0-1

pan :: UGen -> UGen -> UGen
pan u pos =
  let c = mceChannels u
  in balance2 (head c) (c !! 1) pos 1

spin :: UGen -> UGen -> UGen
spin u pos =
  let c = mceChannels u
  in rotate2 (head c) (c !! 1) pos

-- FILTERS

-- u: signal
-- freq: cycles per second
-- rq: reciprocal of Q factor
-- rs: reciprocal of slope
-- db: decibelOffset
-- reso: 0-1
-- reset: trigger

-- loPass u freq rq
loPass :: UGen -> UGen -> UGen -> UGen
loPass = bLowPass

-- hiPass u freq rq
hiPass :: UGen -> UGen -> UGen -> UGen
hiPass = bHiPass

-- pass u freq rq
pass :: UGen -> UGen -> UGen -> UGen
pass = bBandPass

-- notch u freq rq
notch :: UGen -> UGen -> UGen -> UGen
notch = bBandStop

-- loShelf u freq rs db
loShelf :: UGen -> UGen -> UGen -> UGen -> UGen
loShelf = bLowShelf

-- hiShelf u freq rs db
hiShelf :: UGen -> UGen -> UGen -> UGen -> UGen
hiShelf = bHiShelf

-- peak u freq rq db
peak :: UGen -> UGen -> UGen -> UGen -> UGen
peak = bPeakEQ

-- loMoog u freq reso reset
loMoog :: UGen -> UGen -> UGen -> UGen -> UGen
loMoog u freq reso = moogFF u freq (reso * 4)

hiMoog :: UGen -> UGen -> UGen -> UGen -> UGen
hiMoog u freq reso reset = u - loMoog u freq reso reset

-- ECHO/REVERB

-- mix: 0-1
-- room: 0-1
-- damp: 0-1
-- delay: time until next
-- decay: time until -60dB

-- reverb2 u room damp
reverb2 :: UGen -> UGen -> UGen -> UGen
reverb2 u =
  let c = mceChannels u
  in freeVerb2 (head c) (c !! 1) 1

-- delayCubic u delay
delayCubic :: UGen -> UGen -> UGen
delayCubic u = delayC u maxDelay

-- combCubic u delay decay
combCubic :: UGen -> UGen -> UGen -> UGen
combCubic u = combC u maxDelay

-- allpassCubic u delay decay
allpassCubic :: UGen -> UGen -> UGen -> UGen
allpassCubic u = allpassC u maxDelay

-- DYANMICS

-- u: signal
-- v: secondary signal
-- threshold: 0-1
-- slopeBelow: [-1, 1]
-- slopeAbove: [-1, 1]
-- attack: time
-- release: time

-- distortion u
distortion :: UGen -> UGen
distortion = distort

-- dynamicsProcessor u v threshold slopeBelow slopeAbove attack release
dynamicsProcessor :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dynamicsProcessor = compander
