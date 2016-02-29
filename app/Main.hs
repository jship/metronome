{-# LANGUAGE GADTs #-}

module Main where

import Control.Exception
import Control.Wire
import Data.IORef
import Foreign
import Foreign.C
import Paths_metronome
import Prelude hiding ( (.), id )
import qualified SDL
import qualified SDL.Raw.Audio as SDL.Raw
import qualified Data.Vector.Storable.Mutable as MV

-- Netwire stuff (fun!) -------------------------------------------------------

data Config = Config
  { beatsPerMinute  :: !Int
  , beatsPerMeasure :: !Int
  }

data Beat = DownBeat | OtherBeat | NoBeat

metronomeWire :: (Fractional t, HasTime t s, Monad m) => Wire s () m Config Beat
metronomeWire = mkSFN $ \(Config beatsInMin beatsInMeasure) ->
  (DownBeat, noBeatWire beatsInMin
         --> foldl1 (-->) (replicate (beatsInMeasure - 1) otherBeatWire) -- Hack! Dies if beatsInMeasure < 2.
         --> metronomeWire)

otherBeatWire :: (Fractional t, HasTime t s, Monad m) => Wire s () m Config Beat
otherBeatWire = mkSFN $ \(Config beatsInMin beatsInMeasure) ->
  (OtherBeat, noBeatWire beatsInMin)

noBeatWire :: (Fractional t, HasTime t s, Monad m) => Int -> Wire s () m a Beat
noBeatWire beatsInMin = for (60.0 / fromIntegral beatsInMin) . pure NoBeat

-- IO stuff (not fun!) --------------------------------------------------------

main :: IO ()
main = do
  downBeatPath <- getDataFileName "downbeat.wav"
  otherBeatPath <- getDataFileName "otherbeat.wav"
  bracket_ (SDL.initialize $ Just SDL.InitAudio) (SDL.quit) $
    withWAV downBeatPath $ \downBeatSamples ->
    withWAV otherBeatPath $ \otherBeatSamples -> do
    samplesRef <- newIORef []
    bracket (SDL.openAudioDevice . defaultDeviceSpec $ samplesRef)
            (SDL.closeAudioDevice . fst)
            (runWire clockSession_ metronomeWire downBeatSamples otherBeatSamples samplesRef . fst)

runWire :: Session IO s -> Wire s e Identity Config Beat -> [Int16] -> [Int16] -> IORef [Int16] -> SDL.AudioDevice -> IO ()
runWire sIn wIn downBeatSamples otherBeatSamples samplesRef audioDevice = do
  (ds, sOut) <- stepSession sIn
  let Identity (res, wOut) = stepWire wIn ds (Right (Config 60 4))
  either (const . error $ "Unexpected inhibition!")
         (processBeat downBeatSamples otherBeatSamples samplesRef audioDevice)
         res
  runWire sOut wOut downBeatSamples otherBeatSamples samplesRef audioDevice

processBeat :: [Int16] -> [Int16] -> IORef [Int16] -> SDL.AudioDevice -> Beat -> IO ()
processBeat _ _ _ _ NoBeat = return ()
processBeat downBeatSamples otherBeatSamples samplesRef audioDevice beat =
  bracket_ (SDL.setAudioDeviceLocked audioDevice SDL.Locked)
           (SDL.setAudioDeviceLocked audioDevice SDL.Unlocked >>
             SDL.setAudioDevicePlaybackState audioDevice SDL.Play)
           (writeIORef samplesRef $ case beat of
             DownBeat -> downBeatSamples
             OtherBeat -> otherBeatSamples)

audioCB :: IORef [Int16] -> SDL.AudioFormat sampleType -> MV.IOVector sampleType -> IO ()
audioCB samplesRef SDL.Signed16BitLEAudio buffer = do
  samples <- readIORef samplesRef
  let n = MV.length buffer
  MV.set buffer 0 -- Hack! Should be using SDL-computed silence value.
  sequence_ . zipWith (MV.write buffer) [0 ..] . take n $ samples
  writeIORef samplesRef . drop n $ samples
audioCB _ _ _ = error "Unexpected audio format!"

-- Hack! This function assumes signed 16-bit WAV format.
withWAV :: FilePath -> ([Int16] -> IO ()) -> IO ()
withWAV path action = convertSamples <$> load >>= action where
  load = withCString path $ \szPath ->
    alloca $ \desiredSpecPtr ->
    alloca $ \bufferPtr ->
    alloca $ \bufferLenPtr ->
    flip finally (SDL.Raw.freeWAV =<< peek bufferPtr) $ do
    SDL.Raw.loadWAV szPath desiredSpecPtr bufferPtr bufferLenPtr
    bufferLen <- fromIntegral <$> peek bufferLenPtr
    peekArray bufferLen =<< peek bufferPtr

  convertSamples :: [Word8] -> [Int16]
  convertSamples [] = []
  convertSamples (w1 : w2 : ws) = (fromIntegral w1 + 256 * fromIntegral w2)
                                : convertSamples ws

defaultDeviceSpec :: IORef [Int16] -> SDL.OpenDeviceSpec
defaultDeviceSpec initSamples = SDL.OpenDeviceSpec
  { SDL.openDeviceFreq = SDL.Mandate 44100
  , SDL.openDeviceFormat = SDL.Mandate SDL.Signed16BitNativeAudio
  , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
  , SDL.openDeviceSamples = 4096
  , SDL.openDeviceCallback = audioCB initSamples
  , SDL.openDeviceUsage = SDL.ForPlayback
  , SDL.openDeviceName = Nothing
  }
