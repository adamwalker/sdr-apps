{-# LANGUAGE RecordWildCards #-}

import Control.Monad.Trans.Either
import Data.Word
import Data.Complex
import Data.Maybe
import Data.Monoid

import Pipes                as P
import Pipes.Prelude        as P
import Options.Applicative
import Data.Vector.Storable as VS hiding ((++))
import Data.Vector.Generic  as VG hiding ((++))

import SDR.Util             as U
import SDR.RTLSDRStream
import SDR.Filter
import SDR.ArgUtils
import SDR.FilterDesign
import SDR.Pulse
import SDR.PipeUtils
import SDR.CPUID

data Options = Options {
    frequency    :: Word32,
    bandwidth    :: Maybe Int
}

optParser :: Parser Options
optParser = Options 
          <$> option (fmap fromIntegral parseSize) (
                 long "frequency"  
              <> short 'f' 
              <> metavar "FREQUENCY" 
              <> help "Frequency to tune to"
              )
          <*> optional (option (fmap fromIntegral parseSize) (
                 long "bandwidth" 
              <> short 'b' 
              <> metavar "BANDWIDTH" 
              <> help "Filter bandwidth. From 0 to 32K."
              ))

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "Receive AM Radio" <> header "RTLSDR AM")

size = 4096

doIt Options{..} = do

    info            <- lift getCPUInfo

    str             <- sdrStream (defaultRTLSDRParams (frequency + 256000) 1024000) 1 (fromIntegral $ size * 2)

    let coeffsDecim :: [Float]
        coeffsDecim =  VS.toList $ windowedSinc 71 0.4 blackman
    deci            <- lift $ fastDecimatorC info 2 coeffsDecim 

    let coeffsFilt  :: [Float]
        coeffsFilt  =  VS.toList $ windowedSinc 71 (fromIntegral (fromMaybe 16000 bandwidth) / 32000) blackman 
    filt            <- lift $ fastFilterC info coeffsFilt

    let coeffsResp  :: [Float]
        coeffsResp  =  VS.toList $ windowedSinc 71 0.25 blackman 
    resp            <- lift $ fastResamplerR info 3 2 coeffsResp

    pulseSink       <- lift pulseAudioSink

    lift $ runEffect $   str 
                     >-> P.map (interleavedIQUnsignedByteToFloatFast info)
                     >-> P.map (VG.zipWith (*) (quarterBandUp size))
                     >-> firDecimator deci size 
                     >-> firDecimator deci size 
                     >-> firDecimator deci size 
                     >-> firDecimator deci size 
                     >-> firDecimator deci size 
                     >-> firFilter  filt size
                     >-> P.map (VG.map magnitude)
                     >-> dcBlockingFilter
                     >-> firResampler resp size
                     >-> P.map (VG.map (* 6))
                     >-> pulseSink

main = execParser opt >>= eitherT putStrLn return . doIt

