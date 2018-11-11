module IndexPage where

import Sisyphus.Api (IndexPage(..))
import Lucid


instance ToHtml IndexPage where
    toHtml _ = doctypehtml_ $ do
        head_ $ do
            js_ "/static/rts.js"
            js_ "/static/lib.js"
            js_ "/static/out.js"
        body_ $ js_ "/static/runmain.js"
        where
        js_ href = script_ "" `with` [type_ "text/javascript", src_ href]
    toHtmlRaw = toHtml
