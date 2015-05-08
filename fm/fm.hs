{-# LANGUAGE RecordWildCards #-}
import Data.Complex
import Control.Monad.Trans.Either
import Foreign.C.Types
import Data.Word
import System.IO

import Data.Vector.Storable as VS hiding ((++))
import Data.Vector.Generic as VG hiding ((++))
import Pipes
import qualified Pipes.Prelude as P
import Foreign.Storable.Complex

import Options.Applicative

import SDR.Filter 
import SDR.RTLSDRStream
import SDR.Util
import SDR.Demod
import SDR.Pulse
import SDR.Serialize
import SDR.ArgUtils
import SDR.CPUID

--The filter coefficients are stored in another module
import Coeffs

data Options = Options {
    frequency  :: Word32,
    input      :: Maybe FilePath,
    output     :: Maybe FilePath
}

optParser :: Parser Options
optParser = Options 
          <$> option (fmap fromIntegral parseSize) (
                 long "frequency"  
              <> short 'f' 
              <> metavar "FREQUENCY" 
              <> help "Frequency to tune to"
              )
          <*> optional (strOption (
                 long "input"   
              <> short 'i' 
              <> metavar "FILENAME"  
              <> help "Input filename"
              ))
          <*> optional (strOption (
                 long "output"   
              <> short 'o' 
              <> metavar "FILENAME"  
              <> help "Output filename"
              ))

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "Receive and demodulate broadcast FM radio" <> header "RTLSDR FM")

bufNum     = 1
bufLen     = 16384
samples    = fromIntegral (bufNum * bufLen) `quot` 2
decimation = 8
sqd        = samples `quot` decimation

{-
    Sampling frequency of the input is 1280 khz
    This is decimated by a factor of 8 and then demodulated
    Sampling frequency of demodulated signal is 160 khz
    Need audio output at 48 khz
    Resampling factor is 48/160 == 3/10
    FM pilot tone at 19khz (0.3958 * 48)
    Start audio filter cutoff at 15khz (0.3125 * 48)
-}

doIt Options{..} = do

    info <- lift getCPUInfo

    let rtlstream = do
            str <- sdrStream frequency 1280000 bufNum bufLen
            return $ str >-> P.map (convertFast info) 

    let fileStream fname = lift $ do
            h <- openFile fname ReadMode
            return $ fromHandle samples h 
            --TODO: how do I ensure these handles get closed?

    inputSpectrum <- maybe rtlstream fileStream input

    let fileSink fname = do
            h <- openFile fname ReadMode
            return $ toHandle h 

    sink <- lift $ maybe pulseAudioSink fileSink output

    deci <- lift $ fastDecimatorC info decimation coeffsRFDecim 
    resp <- lift $ fastResamplerR info 3 10 coeffsAudioResampler
    filt <- lift $ fastSymmetricFilterR  coeffsAudioFilter

    --Build the pipeline
    let pipeline :: Effect IO ()
        pipeline =   inputSpectrum 
                 >-> firDecimator deci sqd 
                 >-> fmDemod
                 >-> firResampler resp sqd 
                 >-> firFilter filt sqd
                 >-> P.map (VG.map (* 0.2)) 
                 >-> sink

    --Run the pipeline
    lift $ runEffect pipeline

main = execParser opt >>= eitherT putStrLn return . doIt

