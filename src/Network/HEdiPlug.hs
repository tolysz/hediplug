{-# LANGUAGE  OverloadedStrings, BangPatterns, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies, ViewPatterns          #-}

module Network.HEdiPlug where

import qualified Network.Socket.ByteString as SB
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as BS

import Data.Monoid
import Control.Monad
import Control.Concurrent (forkIO,threadDelay)
import qualified Data.Binary as B
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Exception (bracket, SomeException, catch)
import Network.HEdiPlug.Types
import qualified Data.ByteString.Char8 as C8
-- import qualified Data.ByteString.Char8 as C8

import Data.String.QM
import           Yesod

import Data.Time (getCurrentTime)
import Control.Applicative

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit.List (consume)
import Data.Conduit (($$))

-- 2015 02 01 19 30 21

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/smartplug.cgi HomeR POST
|]

instance Yesod HelloWorld

newtype XML = XML C8.ByteString

instance ToContent XML where
    toContent (XML x) = toContent $ x
instance ToTypedContent XML where
    toTypedContent = TypedContent "application/xml; charset=utf-8" . toContent

bss :: Handler L.ByteString
bss = return . L.fromChunks =<<  (rawRequestBody $$ consume )

powerNow      = [qq|<?xml version="1.0" encoding="UTF8"?><SMARTPLUG id="edimax"><CMD id="get"><NOW_POWER><Device.System.Power.NowCurrent></Device.System.Power.NowCurrent><Device.System.Power.NowPower></Device.System.Power.NowPower></NOW_POWER></CMD></SMARTPLUG>|]
powerInfoNow  = [qq|<?xml version="1.0" encoding="UTF8"?><SMARTPLUG id="edimax"><CMD id="get"><NOW_POWER></NOW_POWER></CMD></SMARTPLUG>|]
powerState    = [qq|<?xml version="1.0" encoding="UTF8"?><SMARTPLUG id="edimax"><CMD id="get"><Device.System.Power.State></Device.System.Power.State><Device.System.Power.NextToggle></Device.System.Power.NextToggle></CMD></SMARTPLUG>|]
deviceInitReq = [qq|<?xml version="1.0" encoding="UTF8"?><SMARTPLUG id="edimax"><CMD id="get"><SYSTEM_INFO></SYSTEM_INFO><Device.System.Time></Device.System.Time></CMD></SMARTPLUG>|]
deviceTime    = [qq|<?xml version="1.0" encoding="UTF8"?><SMARTPLUG id="edimax"><CMD id="get"><Device.System.Time></Device.System.Time><SYSTEM_INFO><Device.System.TimeZone.Zone></Device.System.TimeZone.Zone><Device.System.TimeZone.Daylight.Enable></Device.System.TimeZone.Daylight.Enable></SYSTEM_INFO></CMD></SMARTPLUG>|]
powerUsageSet = [qq|<?xml version="1.0" encoding="UTF8"?><SMARTPLUG id="edimax"><CMD id="setup"><POWER_USAGE><Device.System.Power.Budget.UnitPrice>30.0</Device.System.Power.Budget.UnitPrice></POWER_USAGE></CMD></SMARTPLUG>|]
getUnitPrice  = [qq|<?xml version="1.0" encoding="UTF8"?><SMARTPLUG id="edimax"><CMD id="get"><POWER_USAGE><Device.System.Power.Budget.UnitPrice></Device.System.Power.Budget.UnitPrice></POWER_USAGE></CMD></SMARTPLUG>|]

powerHistory = [qq|<?xml version="1.0" encoding="UTF8"?><SMARTPLUG id="edimax"><CMD id="get"><POWER_HISTORY><Device.System.Power.History.Energy unit="HOUR" date="2015011700-2015020122"></Device.System.Power.History.Energy></POWER_HISTORY></CMD></SMARTPLUG>|]
-- <?xml version="1.0" encoding="UTF8"?><SMARTPLUG id="edimax"><CMD id="get"><POWER_HISTORY><Device.System.Power.History.Energy unit="MONTH" date="201101-201502"></Device.System.Power.History.Energy></POWER_HISTORY></CMD></SMARTPLUG>
-- <?xml version="1.0" encoding="UTF8"?><SMARTPLUG id="edimax"><CMD id="get"><POWER_HISTORY><Device.System.Power.History.Energy unit="DAY" date="20140201-20150201"></Device.System.Power.History.Energy></POWER_HISTORY></CMD></SMARTPLUG>


powerHistoryResp = return $ XML $ C8.pack [qq|<?xml version="1.0" encoding="UTF8"?><SMARTPLUG id="edimax"><CMD id="get"><POWER_HISTORY><Device.System.Power.History.Energy unit="HOUR" date="2015011700-2015020122">=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-k-6H-dp-h/-lD-tJ-CN-L+-Vb-12m-1bs-95-eV-kC-qh-vZ-BF-Hk-M/-SI-Yn-121-17I-1dj-1j1-1oH-1un-1An-1Jm-1Sk-1/q-28A-2hM-2mj</Device.System.Power.History.Energy></POWER_HISTORY></CMD></SMARTPLUG>|]

getUnitPriceResp :: Double -> Handler XML
getUnitPriceResp (show -> up) = return $ XML $ C8.pack $ filter ( /='\n') [qn|
<?xml version="1.0" encoding="UTF8"?>
<SMARTPLUG id="edimax">
<CMD id="get">
<POWER_USAGE>
<Device.System.Power.Budget.UnitPrice>${up}</Device.System.Power.Budget.UnitPrice>
</POWER_USAGE>
</CMD>
</SMARTPLUG>|]

deviceTimeResp :: Handler XML
deviceTimeResp = do
  ct <- show . (\a -> FlatTime a :: FlatTime SECOND)  <$> liftIO getCurrentTime -- getTime
  return $ XML $ C8.pack $ filter ( /='\n') [qn|
<?xml version="1.0" encoding="UTF8"?>
<SMARTPLUG id="edimax">
<CMD id="get">
<Device.System.Time>${ct}</Device.System.Time>
<SYSTEM_INFO>
<Device.System.TimeZone.Zone>21</Device.System.TimeZone.Zone>
<Device.System.TimeZone.Daylight.Enable>OFF</Device.System.TimeZone.Daylight.Enable>
</SYSTEM_INFO>
</CMD>
</SMARTPLUG>|]

deviceInitResp :: String -> String -> Handler XML
deviceInitResp mac name = do
 ct <- show . (\a -> FlatTime a :: FlatTime SECOND) <$> liftIO getCurrentTime -- getTime
 return $ XML $ C8.pack $ filter ( /='\n') [qn|
<?xml version="1.0" encoding="UTF8"?>
<SMARTPLUG id="edimax">
<CMD id="get">
<SYSTEM_INFO>
<SUPPORT>
<Device.System.SMTP.Support>1</Device.System.SMTP.Support>
<Device.System.Power.Schedule.Support>1</Device.System.Power.Schedule.Support>
<Device.System.FwUpgrade.Support>1</Device.System.FwUpgrade.Support>
</SUPPORT>
<Run.Cus>Edimax</Run.Cus>
<Run.Model>SP2101W</Run.Model>
<Run.FW.Version>1.03</Run.FW.Version>
<Run.LAN.Client.MAC.Address>${mac}</Run.LAN.Client.MAC.Address>
<Device.System.SMTP.0.Server.Address></Device.System.SMTP.0.Server.Address>
<Device.System.SMTP.0.Server.Port></Device.System.SMTP.0.Server.Port>
<Device.System.SMTP.0.Server.Certificate></Device.System.SMTP.0.Server.Certificate>
<Device.System.SMTP.0.Server.Authorization.Enable></Device.System.SMTP.0.Server.Authorization.Enable>
<Device.System.SMTP.0.Mail.Sender></Device.System.SMTP.0.Mail.Sender>
<Device.System.SMTP.0.Mail.Recipient></Device.System.SMTP.0.Mail.Recipient>
<Device.System.SMTP.0.Mail.Action.Notify.Enable></Device.System.SMTP.0.Mail.Action.Notify.Enable>
<Device.System.SMTP.0.Server.Authorization.Name></Device.System.SMTP.0.Server.Authorization.Name>
<Device.System.SMTP.0.Server.Authorization.Password></Device.System.SMTP.0.Server.Authorization.Password>
<Device.System.TimeZone.Zone>21</Device.System.TimeZone.Zone>
<Device.System.TimeZone.Server.Address.0>pool.ntp.org</Device.System.TimeZone.Server.Address.0>
<Device.System.TimeZone.Server.Address.1>europe.pool.ntp.org</Device.System.TimeZone.Server.Address.1>
<Device.System.TimeZone.Server.Address.2>oceania.pool.ntp.org</Device.System.TimeZone.Server.Address.2>
<Device.System.TimeZone.Server.Address.3>north-america.pool.ntp.org</Device.System.TimeZone.Server.Address.3>
<Device.System.TimeZone.Server.Address.4>south-america.pool.ntp.org</Device.System.TimeZone.Server.Address.4>
<Device.System.TimeZone.Daylight.Enable>OFF</Device.System.TimeZone.Daylight.Enable>
<Device.System.Name>${name}</Device.System.Name>
</SYSTEM_INFO>
<Device.System.Time>${ct}</Device.System.Time>
</CMD>
</SMARTPLUG>
|]
--  liftIO $ BS.putStrLn r
--  r

postHomeR :: Handler XML
postHomeR = do
   let
        mac = "3C970E4445DA"
        name = "FakePlug"
   body <- L8.unpack <$> bss
   liftIO $ putStrLn body
   case () of
     _ | body == deviceInitReq -> -- do
--            liftIO $ print "deviceInitReq"
           deviceInitResp mac name
       | body == powerState -> do
           liftIO $ print "powerState"
           emptyX
       | body == powerNow -> do
           liftIO $ print "powerNow"
           emptyX
       | body == deviceTime -> -- do
--            liftIO $ print "deviceTime"
           deviceTimeResp
       | body == powerInfoNow -> do
            liftIO $ print "powerInfoNow"
            emptyX
       | body == powerUsageSet -> do
            liftIO $ print "powerUsageSet"
            emptyX
       | body == getUnitPrice  -> do
            liftIO $ print "getUnitPrice "
            getUnitPriceResp 0.16
       | otherwise -> do
           liftIO $ print "Unknown"
           powerHistoryResp
--            emptyX
emptyX = return $ XML [qq||]
-- Authorization: Basic YWRtaW46MTIzNA==\r


fakeProxy :: IO ()
fakeProxy = warp 10000 HelloWorld

-- | a discovery UDP port to send to:
discoveryPort = 20560

discoveryMsg :: Discovery ()
discoveryMsg = Discovery "ff:ff:ff:ff:ff:ff" "EDIMAX" 0x00a1ff5e ()

getChanContents :: TChan a -> STM [a]
getChanContents c = do
   b <- isEmptyTChan c
   if b
     then
       return []
     else
       liftM2 (:) (readTChan c) (getChanContents c)

discover :: IO [Discovery PlugInfo]
discover = do
  c <- newTChanIO
  withSocketsDo $ bracket (getSocket 0) sClose (\s -> handler c s `catch` (\e -> let z = (e :: SomeException) in return () ))
  atomically $ getChanContents c

getSocket port = do
        addrinfos <- getAddrInfo
             (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
             Nothing (Just $ show port)
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
        setSocketOption sock Broadcast 1
        bindSocket sock (addrAddress serveraddr) >> return sock

handler ::  TChan (Discovery PlugInfo) -> Socket -> IO ()
handler c conn = do
    let loop conn = do
         (!msg) <- recv conn 1024
         unless (BS.null msg) $ do
                 let !dpi = decodeD msg :: Discovery PlugInfo
                 atomically $ writeTChan c dpi
         loop conn

    (devic:_) <- getAddrInfo Nothing (Just "255.255.255.255") (Just $ show discoveryPort)
    void $ forkIO $ bracket (return conn) (\_ -> return () ) loop
    replicateM_ 1 (SB.sendAllTo conn (encodeD discoveryMsg) (addrAddress devic) >> threadDelay 100000)
    threadDelay 100000

fakePlug =
   void $ forkIO $ withSocketsDo $ bracket (getSocket discoveryPort) sClose fakeId

fakeId conn = do
   (!msg,dip) <- recvFrom conn 1024
--    print msg
--    print $ dBasic msg
   SB.sendAllTo conn (encodeD fakePlugMsg) dip -- (addrAddress devic)
   fakeId conn

fakePlugMsg = Discovery
  "3c:97:0e:44:45:da"
  "EDIMAX"
  0x01a1fe5e
  (PlugInfo "SP2101W" "1.03" "FakePlug" 0x1027 "192.168.1.86" "255.255.255.0" "192.168.1.254")

dBasic msg = decodeD msg :: Discovery ()

{--
"POST /smartplug.cgi HTTP/1.1\r\nAuthorization: Basic YWRtaW46MTIzNA==\r\nConnection: close\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: 300\r\nHost: 192.168.1.86:10000\r\n\r\n<?xml version=\"1.0\" encoding=\"UTF8\"?><SMARTPLUG id=\"edimax\"><CMD id=\"get\"><Device.System.Time></Device.System.Time><SYSTEM_INFO><Device.System.TimeZone.Zone></Device.System.TimeZone.Zone><Device.System.TimeZone.Daylight.Enable></Device.System.TimeZone.Daylight.Enable></SYSTEM_INFO></CMD></SMARTPLUG>"
"HTTP/1.1 200 OK\r\nContent-Type: application/xml; charset=utf-8\r\nCache-Control: no-cache\r\nPragma: no-cache\r\nContent-Length: 319\r\nConnection: close\r\nDate: Sun, 01 Feb 2015 19:24:55 GMT\r\nServer: lighttpd/1.4.31\r\n\r\n<?xml version=\"1.0\" encoding=\"UTF8\"?><SMARTPLUG id=\"edimax\"><CMD id=\"get\"><Device.System.Time>20150201192455</Device.System.Time><SYSTEM_INFO><Device.System.TimeZone.Zone>21</Device.System.TimeZone.Zone><Device.System.TimeZone.Daylight.Enable>OFF</Device.System.TimeZone.Daylight.Enable></SYSTEM_INFO></CMD></SMARTPLUG>"

--}

