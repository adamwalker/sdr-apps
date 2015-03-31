{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.Trans.Either
import System.IO
import Data.Word
import Data.Complex
import Foreign.C.Types

import Foreign.Storable.Complex
import Options.Applicative
import Pipes as P
import Pipes.Prelude as P
import Data.Vector.Storable as VS hiding ((++))

import SDR.Util 
import SDR.RTLSDRStream
import SDR.ArgUtils 
import SDR.Serialize as S

data Options = Options {
    fileName   :: FilePath,
    frequency  :: Word32,
    sampleRate :: Word32,
    size       :: Maybe Int
}

optParser :: Parser Options
optParser = Options 
          <$> strOption (
                 long "output  "   
              <> short 'o' 
              <> metavar "FILENAME"  
              <> help "Output filename"
              )
          <*> option (fmap fromIntegral parseSize) (
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
          <*> optional (option (fmap fromIntegral parseSize) (
                 long "size" 
              <> short 's' 
              <> metavar "SIZE" 
              <> help "Size of output file. If omitted, samples will be recorded until the program is killed"
              ))

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "Record IQ samples from an RTL2832 based device" <> header "RTLSDR Record")

doIt Options{..} = do
    str <- sdrStream frequency sampleRate 1 16384
    lift $ withFile fileName WriteMode $ \handle -> 
        runEffect $ str >-> maybe P.cat P.take size >-> P.map (makeComplexBufferVect 8192 :: VS.Vector CUChar -> VS.Vector (Complex CFloat)) >-> S.toHandle handle

main = execParser opt >>= eitherT putStrLn return . doIt

