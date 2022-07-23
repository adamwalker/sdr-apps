{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

import Control.Monad.Trans.Except
import Data.Complex
import Foreign.C.Types
import Data.Maybe
import Control.Monad

import Control.Error.Util
import Pipes as P
import qualified Pipes.Prelude as P
import Options.Applicative
import Data.Vector.Storable as VS hiding ((++))
import Data.Vector.Generic as VG hiding ((++))

import Network.Socket (SockAddr(SockAddrInet), PortNumber, HostAddress)

import SDR.ArgUtils
import SDR.Util
import SDR.NetworkStream
import SDR.Pulse

data Options = Options {
    ip           :: Maybe HostAddress,
    port         :: Maybe PortNumber
}

optParser :: Parser Options
optParser = Options 
          <$> optional (option auto (
                 long "ip"
              <> short 'i'
              <> metavar "IP"
              <> help "IP address to bind to. Default is 0.0.0.0"
              ))
          <*> optional (option auto (
                 long "port"
              <> short 'p'
              <> metavar "PORT"
              <> help "UDP port to bind to. Default is 0x1234"
              ))

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "Play samples received over Ethernet" <> header "Ethernet to Sound Card")

doIt Options{..} = do
    dev <- lift $ udpRecvSocket $ SockAddrInet (fromMaybe 0x1234 port) (fromMaybe 0 ip)
    let fftSize' = 1024
    sink <- lift pulseAudioSink 

    lift $ runEffect $   udpSource dev fftSize'
                     >-> P.map (VS.map ((/128) . fromIntegral) :: VS.Vector CChar -> VS.Vector Float)
                     >-> sink

main = execParser opt >>= exceptT putStrLn return . doIt

