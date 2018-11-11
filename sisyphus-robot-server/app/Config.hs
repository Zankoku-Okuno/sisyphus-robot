module Config where

import qualified Data.ByteString as BS
import Data.Aeson
import GHC.Generics

import Network.Wai.Handler.Warp (Port)


data Config = Config
    { port :: Port
    , staticDir :: FilePath
    }
    deriving (Show, Generic)

instance ToJSON Config
instance FromJSON Config

readConfig :: FilePath -> IO Config
readConfig filepath = do
    contents <- BS.readFile filepath
    pure $ case decodeStrict' contents of
        Just it -> it
        Nothing -> error $ "invalid config file: " ++ show filepath