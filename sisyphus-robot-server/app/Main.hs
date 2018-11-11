module Main where

import Data.Proxy
import Control.Monad.IO.Class

import System.Environment
import Network.Wai.Handler.Warp
import Servant

import Sisyphus.Api
import Sisyphus.ProductManagement.Pbs
import Config
import IndexPage

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS


main :: IO ()
main = do
    config <- getArgs >>= \case
        [it] -> readConfig it
        _ -> error "ERROR: Usage is: sisyphus-robot-server <config file>"
    putStrLn $ "Config is: " ++ show config
    runServer config

runServer :: Config -> IO ()
runServer Config{..} = do
    putStrLn $ "Serving \"Sisyphus Robot\" on port " ++ show port ++ "..."
    let server = serve (Proxy @TestApi) api
    run port server
    where
    api :: Server TestApi
    api =    fst pbsApi
        :<|> snd pbsApi
        :<|> staticApi
        :<|> indexApi
        where
        pbsApi = (get, put)
            where
            get :: Server ("the" :> Get '[JSON] Pbs)
            get = do
                content <- liftIO $ LBS.readFile "foo"
                case Json.decode content of
                    Nothing -> throwError err500
                    Just pbs -> pure pbs
            put :: Server ("the" :> ReqBody '[JSON] Pbs :> Put '[JSON] NoContent)
            put pbs = do
                liftIO $ LBS.writeFile "foo" $ Json.encode pbs
                pure NoContent
        staticApi = serveDirectoryWebApp staticDir
        indexApi = pure IndexPage
