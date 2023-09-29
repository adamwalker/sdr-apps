{-# LANGUAGE RecordWildCards #-}

import Control.Monad.Trans.Except
import Data.Word
import Data.Int
import Data.Complex
import Foreign.C.Types
import Data.Maybe
import Control.Monad
import Data.Monoid

import Control.Error.Util
import Pipes as P
import Pipes.Prelude as P
import Options.Applicative
import Data.Vector.Storable as VS hiding ((++))
import Data.Vector.Generic as VG hiding ((++))
import Graphics.Rendering.OpenGL

import SDR.Util as U
import SDR.RTLSDRStream
import SDR.FFT
import SDR.Plot
import SDR.ArgUtils
import Graphics.DynamicGraph.Waterfall
import Graphics.DynamicGraph.Util

data Options = Options {
    frequency    :: Word32,
    sampleRate   :: Word32,
    gain         :: Maybe Int32,
    fftSize      :: Maybe Int,
    windowWidth  :: Maybe Int,
    windowHeight :: Maybe Int,
    rows         :: Maybe Int,
    colorMap     :: Maybe [GLfloat]
}

parseColorMap :: ReadM [GLfloat]
parseColorMap = eitherReader func
    where
    func "jet"     = return jet
    func "jet_mod" = return jet_mod
    func "hot"     = return hot
    func "bw"      = return bw
    func "wb"      = return wb
    func arg       = Left $ "Cannot parse colour map: `" ++ arg ++ "'"

optParser :: Parser Options
optParser = Options 
          <$> option (fmap fromIntegral parseSize) (
                 long "frequency"  
              <> short 'f' 
              <> metavar "FREQUENCY" 
              <> help "Frequency to tune to"
              )
          <*> option (fmap fromIntegral parseSize) (
                 long "samplerate" 
              <> short 'r' 
              <> metavar "RATE" 
              <> help "Sample rate"
              )
          <*> optional (option auto (
                 long "gain" 
              <> short 'g' 
              <> metavar "GAIN" 
              <> help "Tuner gain"
              ))
          <*> optional (option (fmap fromIntegral parseSize) (
                 long "size" 
              <> short 's' 
              <> metavar "SIZE" 
              <> help "FFT bin size. Default is 8192."
              ))
          <*> optional (option auto (
                 long "width" 
              <> short 'w' 
              <> metavar "WIDTH" 
              <> help "Window width. Default is 1024."
              ))
          <*> optional (option auto (
                 long "vertSize" 
              <> short 'v' 
              <> metavar "VERTSIZE" 
              <> help "Window height. Default is 480."
              ))
          <*> optional (option auto (
                 long "lines" 
              <> short 'l' 
              <> metavar "LINES" 
              <> help "Number of rows in waterfall. Default is 1000."
              ))
          <*> optional (option parseColorMap (
                 long "colorMap" 
              <> short 'm' 
              <> metavar "COLORMAP" 
              <> help "Waterfall color map. Default is 'jet_mod'."
              ))

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "Draw a dynamic waterall plot of the received spectrum using OpenGL" <> header "RTLSDR Waterfall")

doIt Options{..} = do
    res <- lift setupGLFW
    unless res (throwE "Unable to initilize GLFW")

    let fftSize' =  fromMaybe 8192 fftSize
        window   =  hanning fftSize' :: VS.Vector Double
    str          <- sdrStream ((defaultRTLSDRParams frequency sampleRate) {tunerGain = gain}) 1 (fromIntegral $ fftSize' * 2)
    rfFFT        <- lift $ fftw fftSize'
    rfSpectrum   <- plotWaterfall (fromMaybe 1024 windowWidth) (fromMaybe 480 windowHeight) fftSize' (fromMaybe 1000 rows) (fromMaybe jet_mod colorMap)
    --rfSpectrum   <- plotFill (maybe 1024 id windowWidth) (maybe 480 id windowHeight) fftSize' (maybe jet_mod id colorMap)
    --rfSpectrum   <- plotTexture (maybe 1024 id windowWidth) (maybe 480 id windowHeight) fftSize' fftSize'

    lift $ runEffect $   str 
                     >-> P.map (interleavedIQUnsigned256ToFloat :: VS.Vector CUChar -> VS.Vector (Complex Double)) 
                     >-> P.map (VG.zipWith (flip mult) window . VG.zipWith mult (halfBandUp fftSize')) 
                     >-> rfFFT 
                     >-> P.map (VG.map ((* (32 / fromIntegral fftSize')) . realToFrac . magnitude)) 
                     >-> rfSpectrum 

main = execParser opt >>= exceptT putStrLn return . doIt



db20 x = 20 * log x / (log 10)
