module Sisyphus.ProductManagement.Pbs
    ( Pbs(..)
    , PbsRecord(..)
    , Code, Codename, HierCode, FlatCode
    , freshPbs
    , addFreshAssembly
    , addFreshPart
    , removeByCode
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

removeByCode :: Code -> Pbs -> Maybe Pbs
removeByCode targetCode t@Tree{} = do
    ix <- findIndex (\sub -> targetCode == (code . _info) sub) (_subtrees t)
    let (pre, post) = splitAt ix (_subtrees t)
        subtrees' = pre ++ drop 1 post
    pure $ renumber t{_subtrees = subtrees'}
removeByCode targetCode l@Leaves{} = do
    ix <- findIndex (\r -> targetCode == code r) (_records l)
    let (pre, post) = splitAt ix (_records l)
        records' = pre ++ drop 1 post
    pure $ renumber l{_records = records'}
-- TODO
-- updateByCode (i.e. update _info)
-- hide the internals behind this interface so I can control Pbs invariants

renumber :: Pbs -> Pbs
renumber pbs = go (fst . code $ _info pbs) pbs
    where
    go code0 t@Tree{} = Tree
        { _info = (_info t){ code = (code0, Nothing) }
        , _subtrees = zipWith (countHier code0) [1..] (_subtrees t)
        }
    go code0 l@Leaves{} = Leaves
        { _info = (_info l){ code = (code0, Nothing) }
        , _records = zipWith (countFlat code0) [1..] (_records l)
        }
    countHier code0 i pbs = go (code0 ++ [i]) pbs
    countFlat code0 i r = r{ code = (code0, Just i) }

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
