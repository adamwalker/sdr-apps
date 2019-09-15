{-# LANGUAGE RecordWildCards #-}
import Data.Word
import Data.Int
import Data.Maybe
import Control.Error.Util

import Pipes as P
import Pipes.Prelude as P
import Options.Applicative
import Data.Vector.Generic as VG hiding ((++))

import Network.Socket (SockAddr(SockAddrInet), tupleToHostAddress, PortNumber, HostAddress)

import SDR.RTLSDRStream
import SDR.ArgUtils
import SDR.NetworkStream

data Options = Options {
    frequency    :: Word32,
    sampleRate   :: Word32,
    gain         :: Maybe Int32,
    size         :: Maybe Int,
    ip           :: Maybe HostAddress,
    port         :: Maybe PortNumber
}

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
              <> help "FFT bin size. Default is 512."
              ))
          <*> optional (option auto (
                 long "ip"
              <> short 'i'
              <> metavar "IP"
              <> help "IP address to send to. Default is 192.168.5.2"
              ))
          <*> optional (option auto (
                 long "port"
              <> short 'p'
              <> metavar "PORT"
              <> help "UDP port to send to. Default is 0x1234"
              ))

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "Stream samples to Ethernet" <> header "RTLSDR Ethernet stream")

doIt Options{..} = do

    let fftSize' =  fromMaybe 512 size
    str          <- sdrStream ((defaultRTLSDRParams frequency sampleRate) {tunerGain = gain}) 1 (fromIntegral $ fftSize' * 2)

    dev <- lift udpSendSocket

    lift $ runEffect 
        $   str 
        >-> P.map (VG.map (\x -> x - 128))
        >-> udpSink dev (SockAddrInet (fromMaybe 0x1234 port) (fromMaybe (tupleToHostAddress (192, 168, 5, 2)) ip))

main = execParser opt >>= exceptT putStrLn return . doIt

