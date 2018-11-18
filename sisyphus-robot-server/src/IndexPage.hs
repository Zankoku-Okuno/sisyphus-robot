module IndexPage where

import Sisyphus.Api (IndexPage(..))
import Lucid


instance ToHtml IndexPage where
    toHtml _ = doctypehtml_ $ do
        head_ $ do
            title_ "Sisyphus Robot"
            js_ "/static/app/rts.js"
            js_ "/static/app/lib.js"
            js_ "/static/app/out.js"
            css_ "/static/layout.css"
        body_ $ js_ "/static/app/runmain.js"
        where
        js_ href = script_ "" `with` [type_ "text/javascript", src_ href]
        css_ href = link_ [rel_ "stylesheet", type_ "text/css", href_ href]
    toHtmlRaw = toHtml
