module Main where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson as Json
import Reflex.Dom

import Sisyphus.Api
import Sisyphus.ProductManagement.Pbs


main :: IO ()
main = mainWidgetWithHead elHead elBody

elHead :: (MonadWidget t m) => m ()
elHead = do
    -- elAttr "meta" ("charset" =: "utf-8") blank
    el "title" $ text "Sisyphus Robot"

elBody :: (MonadWidget t m) => m ()
elBody = do
    el "div" $ do
        dynPbs <- elLoad
        elSave dynPbs
        el "table" $ do
            el "thead" $ el "tr" $ do
                el "th" $ text "Code"
                el "th" $ text "Name"
            el "tbody" $ do
                let elRecord dynR = el "tr" $ do
                        el "td" $ dynText (displayCode <$> dynPbs <*> dynR)
                        el "td" $ dynText (name <$> dynR)
                simpleList (records <$> dynPbs) elRecord
                blank
    where
    elLoad :: (MonadWidget t m) => m (Dynamic t Pbs)
    elLoad = do
        evPostBuild <- getPostBuild
        evLoadBtn <- button "Load from server"
        evLoadRsp <- getThe $ leftmost [evLoadBtn, evPostBuild]
        foldDyn (\n p -> maybe p id n) def evLoadRsp
    elSave :: (MonadWidget t m) => Dynamic t Pbs -> m (Event t ())
    elSave dynPbs = do
        evSaveBtn <- button "Save to server"
        putThe $ tagPromptlyDyn dynPbs evSaveBtn


getThe :: (MonadWidget t m) => Event t () -> m (Event t (Maybe Pbs))
getThe ev = do
    rsp <- performRequestAsync $ build_getThe <$ ev
    pure $ from_getThe <$> rsp

build_getThe :: XhrRequest ()
build_getThe = XhrRequest "GET" "/the" def

from_getThe :: XhrResponse -> Maybe Pbs
from_getThe rsp = decodeText =<< _xhrResponse_responseText rsp

putThe :: (MonadWidget t m) => Event t Pbs -> m (Event t ())
putThe ev = do
    rsp <- performRequestAsync $ build_putThe <$> ev
    pure $ () <$ rsp

build_putThe :: Pbs -> XhrRequest Text
build_putThe pbs = postJson "/the" pbs
