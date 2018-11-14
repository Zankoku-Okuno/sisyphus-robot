module Sisyphus.ProductManagement.Pbs
    ( Pbs(..)
    , PbsRecord(..)
    , freshPbs
    , addFreshAssembly
    , addFreshPart
    , displayCode
    ) where

import Data.List
import Data.Monoid
import Control.Monad
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics

import Data.Function

type Codename = ()
type HierCode = [Word]
type FlatCode = Word
type Code = (HierCode, Maybe FlatCode)


data Pbs
    = Tree   { _info :: PbsRecord, _subtrees :: [Pbs] }
    | Leaves { _info :: PbsRecord, _records :: [PbsRecord] }
    deriving(Generic, Read, Show)
instance ToJSON Pbs
instance FromJSON Pbs

data PbsRecord = PbsRecord
    { maxFlatCode :: Word
    , code :: Code
    , name :: Text
    }
    deriving(Generic, Read, Show)
instance ToJSON PbsRecord
instance FromJSON PbsRecord




addFreshPart :: Pbs-> Maybe Pbs
addFreshPart Leaves{..} = do
    let partNo' = fromIntegral (length _records) + 1
    guard $ partNo' <= maxFlatCode _info
    let record' = PbsRecord
            { maxFlatCode = maxFlatCode _info
            , code = (hierCode _info, Just partNo')
            , name = ""
            }
        _records' = _records ++ [record']
    pure $ Leaves _info _records'
addFreshPart Tree{_subtrees = [], ..} = do
    addFreshPart $ Leaves _info []
addFreshPart _ = Nothing
addFreshAssembly :: Pbs-> Maybe Pbs
addFreshAssembly Tree{..} = do
    let asmNo' = fromIntegral (length _subtrees) + 1
        subtree' = Tree
            { _info = PbsRecord
                { maxFlatCode = maxFlatCode _info
                , code = (hierCode _info ++ [asmNo'], Nothing)
                , name = ""
                }
            , _subtrees = []
            }
        subtrees' = _subtrees ++ [subtree']
    pure $ Tree _info subtrees'
addFreshAssembly _ = Nothing
-- TODO
-- removeByCode
-- updateByCode
-- hide the internals behind this interface so I can control Pbs invariants


hierCode = fst . code
flatCode = snd . code


freshPbs :: Codename -> Text -> Pbs
freshPbs codename name = Tree info []
    where
    info = PbsRecord
        { maxFlatCode = 999
        , code = ([], Nothing)
        , name = name
        }


displayCode :: PbsRecord -> Text
displayCode info = renderHier (hierCode info) <> renderFlat (flatCode info)
    where
    renderHier [] = ""
    renderHier hier = T.intercalate "." (T.pack . show <$> hier)
    renderFlat Nothing = ""
    renderFlat (Just flat) = "-" <> padLeft '0' (length . show $ maxFlatCode info) (show flat)
    padLeft c len str = T.pack $ replicate (fromIntegral len - length str) c <> str
