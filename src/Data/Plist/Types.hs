{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Plist.Types (PList(..)) where
import           Data.Aeson          (ToJSON (..))
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS8
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import           Data.Time           (UTCTime, defaultTimeLocale, formatTime)
import           GHC.Generics        (Generic)

data PList = String  !Text
           | Integer !Integer
           | Real    !Double
           | Bool    !Bool
           | Date    !UTCTime
           | Data    ByteString
           | Array   [PList]
           | Dict    (HashMap Text PList)
           deriving (Read, Show, Eq, Generic)

instance ToJSON PList where
  toJSON (String txt) = toJSON txt
  toJSON (Integer i)  = toJSON i
  toJSON (Real r)     = toJSON r
  toJSON (Bool b)     = toJSON b
  toJSON (Date d)     = toJSON $ formatTime defaultTimeLocale "%FT%X%Z" d
  toJSON (Data b)     = toJSON $ BS8.unpack b
  toJSON (Array arr)  = toJSON $ map toJSON arr
  toJSON (Dict dic)   = toJSON $ fmap toJSON dic

