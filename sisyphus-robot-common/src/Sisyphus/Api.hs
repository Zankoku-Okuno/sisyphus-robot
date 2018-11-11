module Sisyphus.Api where

import Sisyphus.ProductManagement.Pbs

import Data.Text (Text)
import Servant.API
import Servant.HTML.Lucid


type TestApi
    =    "the" :> Get '[JSON] Pbs
    :<|> "the" :> ReqBody '[JSON] Pbs :> Post '[JSON] NoContent
    :<|> "static" :> Raw
    :<|> Get '[HTML] IndexPage


-- FIXME define this type in the common package, along with its ToHtml
data IndexPage = IndexPage
