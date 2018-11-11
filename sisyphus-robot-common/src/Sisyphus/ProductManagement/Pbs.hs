module Sisyphus.ProductManagement.Pbs where

import Data.List
import Data.Default
import Data.Text (Text)
import Data.Aeson
import GHC.Generics

import Data.Function


data Pbs = Pbs
    { maxFlatCode :: Word
    , records :: [PbsRecord] -- FIXME use Seq; also make sure this is always ordered
    }
    deriving(Generic, Read, Show)
instance ToJSON Pbs
instance FromJSON Pbs

data PbsRecord = PbsRecord
    { hierCode :: [Word]
    , flatCode :: Word
    , name :: Text
    }
    deriving(Generic, Read, Show)
instance ToJSON PbsRecord
instance FromJSON PbsRecord

instance Eq PbsRecord where
    (==) = (==) `on` code
instance Ord PbsRecord where
    compare = compare `on` code


code :: PbsRecord -> ([Word], Word)
code PbsRecord{..} = (hierCode, flatCode)

displayCode :: Pbs -> PbsRecord -> String
displayCode Pbs{..} PbsRecord{..} =
    intercalate "." (show <$> hierCode) ++ "-" ++ padLeft '0' (length . show $ maxFlatCode) (show flatCode)
    where
    padLeft c len str = replicate (fromIntegral len - length str) c ++ str

instance Default Pbs where
    def = Pbs
        { maxFlatCode = 9999
        , records = []
        }