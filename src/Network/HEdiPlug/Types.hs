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
           #-}

module Network.HEdiPlug.Types where

import Control.Lens        (makeLenses)
import GHC.Generics        (Generic)
import Data.Typeable       (Typeable)
import qualified Data.Text as T
import qualified Data.Text.Encoding  as T
import Network.Info
import Data.Word
import Data.Binary
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Data.Monoid
import Control.Applicative
import GHC.TypeLits
import qualified Data.ByteString as BS
import Control.Monad
import Data.Coerce

import Text.Printf

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

discoveryMsg' :: Discovery ()
discoveryMsg' = Discovery (MAC 0xff 0xff 0xff 0xff 0xff 0xff) (CString "EDIMAX") 0x00a1ff5e ()

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
