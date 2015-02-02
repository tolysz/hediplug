{-# Language DataKinds
           , OverloadedStrings
           , RecordWildCards
           , DeriveDataTypeable
           , GeneralizedNewtypeDeriving
           , DeriveGeneric
           , TemplateHaskell
           , KindSignatures
           , RecursiveDo
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts, UndecidableInstances
           #-}

module Network.HEdiPlug.Types
( -- * Types
  MAC (..)
, Hex (..)
, IPv4 (..)
, CString (..)
, Discovery (..)
, PlugInfo (..)
-- * Fun with tine
, FlatTime (..)
, DAY
, MONTH
, HOUR
, MINUTE
, SECOND

, decodeD
, encodeD
, decodeB64Array
, decodeI
, decodeI64
, encodeB64Array
, liftTime
)
 where

import Control.Lens        (makeLenses)
import GHC.Generics        (Generic)
import Data.Typeable       (Typeable)
import qualified Data.Text as T
import qualified Data.Text.Encoding  as T
import Network.Info
import Data.Word
import Data.Binary as B
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Data.String
import Data.Monoid
import Control.Applicative
import GHC.TypeLits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Control.Monad
import Data.Coerce
import Data.Maybe
import qualified Data.List as DL (elemIndex, intercalate)

import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format
import System.Locale


import qualified Codec.Binary.Base64.String as S64

import Text.Printf
import Data.List.Split (splitOn)

newtype Hex a = Hex a deriving (Num)
instance Show (Hex Word32) where
  show (Hex a) = printf "0x%08x" a
instance Show (Hex Word16) where
  show (Hex a) = printf "0x%04x" a

instance Binary a => Binary (Hex a) where
  get = Hex <$> get
  put (Hex a)= put a

data CString (n:: Nat) = CString T.Text
     deriving (Show , Typeable, Generic)

instance KnownNat n => IsString (CString n) where
  fromString a = let r = CString . fromString . take (fromInteger $ natVal r) $ a in r

instance IsString IPv4 where
  fromString s  = IPv4 $ a * 2 ^ 24 +  b * 2 ^ 16 + c * 2 ^ 8 +  d
     where
       [a,b,c,d] = map read $ splitOn "." s

instance IsString MAC where
  fromString s  = MAC a b c d e f
     where
       [a,b,c,d,e,f] = map (read . (++) "0x" ) $ splitOn ":" s


data Discovery a = Discovery
  { _dMAX  :: !MAC
  , _dName :: !(CString 12)
  , _dCMD  :: !(Hex Word32)
  , _dPayload :: !a
  } deriving  (Show, Typeable, Generic)

instance (Binary MAC) where
  put ( MAC a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
  get = MAC <$> get <*> get <*> get <*> get <*> get <*> get

instance (Binary IPv4) where
  put (IPv4 a) = put a
  get = IPv4 <$> getWord32le

instance KnownNat n => Binary (CString n) where
  put a@(CString t) = mapM_ put $ take (fromInteger $ natVal a) $ BS.unpack (T.encodeUtf8 t) <> repeat 0
  get =
    CString . T.decodeUtf8 . BS.pack . takeWhile (/= 0) <$> replicateM (fromInteger $ natVal (undefined :: CString n)) get


makeLenses  ''Discovery
instance Binary a => Binary (Discovery a) where

data PlugInfo = PlugInfo
  { _piModel :: !(CString 14)
  , _piVer   :: !(CString 8)
  , _piName  :: !(CString 128)
  , _piMagic :: !(Hex Word16)
  , _piIP    :: !IPv4
  , _piMask  :: !IPv4
  , _piGW    :: !IPv4
  } deriving (Show, Typeable, Generic)
makeLenses  ''PlugInfo

instance Binary PlugInfo where
 get = PlugInfo <$> get <*> get <*> get <*> get <*> get <*> get <*> get

decodeD :: B.Binary a =>  BS.ByteString -> a
decodeD = B.decode . BSL.fromChunks . (:[])

encodeD :: B.Binary a =>  a -> BS.ByteString
encodeD = head . BSL.toChunks . B.encode

--

decodeB64Array :: String -> [Int]
decodeB64Array s = map decodeI64 $ splitOn "-" s
encodeB64Array :: [Int] -> String
encodeB64Array = DL.intercalate "-" . map encodeI64
lookupTable = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+/"

decodeI a = fromMaybe 0 $ DL.elemIndex a lookupTable

decodeI64 [] = 0
decodeI64 b = decodeI (last b) + 64 * decodeI64 (init b)


encodeI64 :: Int -> String
encodeI64 0 = "="
encodeI64 a | a > 0 = encodeI641 a
encodeI64 _ = error "Need to be > 0"

encodeI641 a = case a `divMod` 64 of
   (0,0) -> ""
   (d,b) -> encodeI641 d ++ [lookupTable !! b]





-- formatT = formatTime defaultTimeLocale "%Y%m%d%H%M%S"
newtype FlatTime a = FlatTime UTCTime

data MONTH
data DAY
data HOUR
data MINUTE
data SECOND

class TimeResolution a where
 tfString :: a -> String

instance TimeResolution (FlatTime MONTH) where
 tfString _ =  "%Y%m"
instance TimeResolution (FlatTime DAY )    where
 tfString _ = "%Y%m%d"
instance TimeResolution (FlatTime HOUR )  where
 tfString _ = "%Y%m%d%H"
instance TimeResolution (FlatTime MINUTE) where
 tfString _ = "%Y%m%d%H%M"
instance TimeResolution (FlatTime SECOND )where
 tfString _ = "%Y%m%d%H%M%S"

-- showTime :: TimeResolution (FlatTime a) => FlatTime a -> String
-- showTime  = show

instance TimeResolution (FlatTime a) => Show (FlatTime a) where
  show xf@(FlatTime x) = formatTime defaultTimeLocale (tfString xf) x

liftTime :: a -> UTCTime -> (FlatTime a)
liftTime _ = FlatTime