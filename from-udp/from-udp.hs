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
import Graphics.Rendering.OpenGL

import Network.Socket (SockAddr(SockAddrInet), PortNumber, HostAddress)

import SDR.FFT
import SDR.Plot
import SDR.ArgUtils
import SDR.Util
import SDR.NetworkStream
import Graphics.DynamicGraph.Waterfall
import Graphics.DynamicGraph.Util

data Options = Options {
    fftSize      :: Maybe Int,
    windowWidth  :: Maybe Int,
    windowHeight :: Maybe Int,
    rows         :: Maybe Int,
    colorMap     :: Maybe [GLfloat],
    ip           :: Maybe HostAddress,
    port         :: Maybe PortNumber
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
          <$> optional (option (fmap fromIntegral parseSize) (
                 long "size" 
              <> short 's' 
              <> metavar "SIZE" 
              <> help "FFT bin size. Default is 512."
              ))
          <*> optional (option auto (
                 long "width" 
              <> short 'w' 
              <> metavar "WIDTH" 
              <> help "Window width. Default is 1024."
              ))
          <*> optional (option auto (
                 long "height" 
              <> short 'h' 
              <> metavar "HEIGHT" 
              <> help "Window height. Default is 480."
              ))
          <*> optional (option auto (
                 long "rows" 
              <> short 'r' 
              <> metavar "ROWS" 
              <> help "Number of rows in waterfall. Default is 1000."
              ))
          <*> optional (option parseColorMap (
                 long "colorMap" 
              <> short 'm' 
              <> metavar "COLORMAP" 
              <> help "Waterfall color map. Default is 'jet_mod'."
              ))
          <*> optional (option auto (
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
opt = info (helper <*> optParser) (fullDesc <> progDesc "Draw a waterall plot of the samples received over Ethernet" <> header "Ethernet Waterfall")

{-# INLINE interleavedIQSigned256ToFloat #-}
interleavedIQSigned256ToFloat :: (Num a, Integral a, Num b, Fractional b, VG.Vector v1 a, VG.Vector v2 (Complex b)) => v1 a -> v2 (Complex b)
interleavedIQSigned256ToFloat input = VG.generate (VG.length input `quot` 2) convert
    where
    {-# INLINE convert #-}
    convert idx  = convert' (input `VG.unsafeIndex` (2 * idx)) :+ convert' (input `VG.unsafeIndex` (2 * idx + 1))
    {-# INLINE convert' #-}
    convert' val = fromIntegral val / 256

doIt Options{..} = do
    res <- lift setupGLFW
    unless res (throwE "Unable to initilize GLFW")

    let fftSize' =  fromMaybe 512 fftSize
        window   =  hanning fftSize' :: VS.Vector Double
    rfFFT        <- lift $ fftw fftSize'
    rfSpectrum   <- plotWaterfall (fromMaybe 1024 windowWidth) (fromMaybe 480 windowHeight) fftSize' (fromMaybe 1000 rows) (fromMaybe jet_mod colorMap)

    dev          <- lift $ udpRecvSocket $ SockAddrInet (fromMaybe 0x1234 port) (fromMaybe 0 ip)

    lift $ runEffect $   udpSource dev (fftSize' * 2)
                     >-> P.map (interleavedIQSigned256ToFloat :: VS.Vector CChar -> VS.Vector (Complex Double)) 
                     >-> P.map (VG.zipWith (flip mult) window . VG.zipWith mult (halfBandUp fftSize')) 
                     >-> rfFFT 
                     >-> P.map (VG.map ((* (32 / fromIntegral fftSize')) . realToFrac . magnitude)) 
                     >-> rfSpectrum 

main = execParser opt >>= exceptT putStrLn return . doIt

