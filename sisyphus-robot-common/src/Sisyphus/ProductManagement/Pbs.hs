module Sisyphus.ProductManagement.Pbs
    ( Pbs(..)
    , PbsNode(..)
    , PbsRecord(..)
    , PbsCtx(..)
    , Code, Codename, HierCode, FlatCode, emptyCode
    , freshPbs
    , addFreshAssembly
    , addFreshPart
    , removeByIndex
    , children
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
type HierCode = [HierCodePart]
type HierCodePart = Word
type FlatCode = Word
type Code = (HierCode, Maybe FlatCode)


emptyCode = ([], Nothing)
hierCode = fst . code
flatCode = snd . code

childCode :: Code -> Either Word FlatCode -> Code
childCode (hier, Nothing) (Left i)  = (hier ++ [i], Nothing)
childCode (hier, Nothing) (Right i) = (hier, Just i)


data Pbs = Pbs
    { ctx :: PbsCtx
    , the :: PbsNode
    }
    deriving(Generic, Read, Show)
instance ToJSON Pbs
instance FromJSON Pbs

data PbsCtx = PbsCtx
    { maxFlatCode :: Word
    , code :: Code
    }
    deriving(Generic, Read, Show)
instance ToJSON PbsCtx
instance FromJSON PbsCtx

data PbsNode
    = Top    { _record :: PbsRecord, _subassemblies :: [PbsNode] } -- FIXME move the _record up into context
    | Tree   { _record :: PbsRecord, _subassemblies :: [PbsNode] }
    | Leaves { _record :: PbsRecord, _parts :: [PbsNode] }
    | Leaf   { _record :: PbsRecord }
    deriving(Generic, Read, Show)
instance ToJSON PbsNode
instance FromJSON PbsNode

data PbsRecord = PbsRecord
    { name :: Text
    }
    deriving(Generic, Read, Show)
instance ToJSON PbsRecord
instance FromJSON PbsRecord


children :: Pbs -> [Pbs]
children Pbs{..} = case the of
    Top{..}    -> zipWith go (Left  <$> [1..]) _subassemblies
    Tree{..}   -> zipWith go (Left  <$> [1..]) _subassemblies
    Leaves{..} -> zipWith go (Right <$> [1..]) _parts
    Leaf{}     -> []
    where
    go code' the = Pbs
        { ctx = ctx{ code = code ctx `childCode` code' }
        , ..
        }


freshPbs :: Codename -> Text -> Pbs
freshPbs codename name = Pbs{..}
    where
    ctx = PbsCtx { maxFlatCode = 999, code = ([], Nothing) }
    the = Top
        { _record = PbsRecord { name = name }
        , _subassemblies = []
        }

addFreshAssembly :: Pbs-> Maybe Pbs
addFreshAssembly Pbs{..} = do
    the' <- case the of
        Top{..}  -> Just Top{ _subassemblies = addKid _subassemblies, .. }
        Tree{..} -> Just Tree{ _subassemblies = addKid _subassemblies, .. }
        Leaves{..} -> do
            guard $ null _parts
            Just $ Tree{ _subassemblies = addKid [], .. }
        -- TODO if an empty leaves, then xmute to Tree
        _        -> Nothing
    pure Pbs{ the = the', .. }
    where
    addKid :: [PbsNode] -> [PbsNode]
    addKid _subassemblies =
        let asmNo' = fromIntegral (length _subassemblies) + 1
            subtree' = Tree
                { _record = PbsRecord { name = "" }
                , _subassemblies = []
                }
        in _subassemblies ++ [subtree']

addFreshPart :: Pbs -> Maybe Pbs
addFreshPart Pbs{ the = Leaves{..}, .. } = do
    let partNo' = fromIntegral (length _parts) + 1
    guard $ partNo' <= maxFlatCode ctx
    let record' = PbsRecord { name = "" }
        _parts' = _parts ++ [Leaf record']
    pure $ Pbs { the = Leaves { _parts = _parts', .. }, .. }
addFreshPart Pbs { the = Tree{..}, .. } = do
    guard $ null _subassemblies
    addFreshPart $ Pbs { the = Leaves{ _parts = [], ..}, .. }
addFreshPart _ = Nothing

removeByIndex :: Int -> Pbs -> Maybe Pbs
removeByIndex ix pbs = do
    guard $ 0 <= ix && ix < length (children pbs)
    let the' = case the pbs of
            Top{..}    -> Top{ _subassemblies = deleteIndex ix _subassemblies, .. }
            Tree{..}   -> Tree{ _subassemblies = deleteIndex ix _subassemblies, .. }
            Leaves{..} -> Leaves{ _parts = deleteIndex ix _parts, .. }
            Leaf{..} -> undefined
    pure pbs{ the = the' }
    where
    deleteIndex i xs =
        let (pre, post) = splitAt i xs
        in pre ++ drop 1 post


displayCode :: PbsCtx -> Text
displayCode ctx = renderHier (hierCode ctx) <> renderFlat (flatCode ctx)
    where
    renderHier [] = ""
    renderHier hier = T.intercalate "." (T.pack . show <$> hier)
    renderFlat Nothing = ""
    renderFlat (Just flat) = "-" <> padLeft '0' (length . show $ maxFlatCode ctx) (show flat)
    padLeft c len str = T.pack $ replicate (fromIntegral len - length str) c <> str
