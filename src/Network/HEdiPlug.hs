{-# LANGUAGE  OverloadedStrings #-}
module Network.HEdiPlug where

-- import  Network.Socket
import qualified Network.Socket.ByteString as SB
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Exception
import qualified Data.ByteString as BS

import Data.Monoid
import Control.Monad
import Control.Concurrent
import qualified Data.Binary as B

import Network.HEdiPlug.Types

-- | a discovery UDP port to send to:
discoveryPort = 20560

-- | we broadcast this one and have clients responding with their info
-- discoveryMsg = BS.pack [0xff,0xff,0xff,0xff,0xff,0xff,0x45,0x44,0x49,0x4d,0x41,0x58,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xa1,0xff,0x5e]


discoveryMsg :: Discovery ()
discoveryMsg = Discovery (MAC 0xff 0xff 0xff 0xff 0xff 0xff) (CString "EDIMAX") 0x00a1ff5e ()

discover :: IO ()
discover = withSocketsDo $ bracket getSocket sClose handler
        where getSocket = do
                addrinfos <- getAddrInfo
                                     (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                     Nothing (Just "0")
                let serveraddr = head addrinfos
                print discoveryMsg
                sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                setSocketOption sock Broadcast  1 -- :: Socket -> SocketOption -> Int -> IO ()
                sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
                setSocketOption sock Broadcast  1
                bindSocket sock (addrAddress serveraddr) >> return sock

handler :: Socket -> IO ()
handler conn = do
    (devic:_) <- getAddrInfo Nothing (Just "255.255.255.255") (Just $ show discoveryPort)

    let loop conn = do
         (msg,d) <- recvFrom conn 1024
         print (decodeD msg :: Discovery PlugInfo )
         loop conn
    void $ forkIO $ loop conn
    replicateM_ 1 (SB.sendAllTo conn (encodeD discoveryMsg) (addrAddress devic) >> threadDelay 500000)

--         MAC W8x6
-- [116,218,56,0,0,1
--   E  D  I  M  A  X  char 12
-- , 69,68,73,77,65,88,0,0,0,0,0,0
--  command w8(4)
-- , 1 ,161,254,94
-- payload::
-- S  P   2  1  0  1  W  char(14)
-- ,83,80,50,49,48,49,87,0,0,0,0,0,0,0
--   1  .  0  3  char(8)
-- ,49,46,48,51,0,0,0,0
--  U    p   s  S  a   f   e char (256)
-- ,85,112,115,83,97,102,101,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- -- ,0,0,0,0,0,0,0,0
-- some magic (crc16?)
-- ,16,39
--  IP
-- ,192,168,1,87
-- Mask
-- ,255,255,255,0
-- GW
-- ,192,168,1,254]
