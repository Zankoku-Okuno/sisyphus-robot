module Main where

import Data.Maybe
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson as Json
import Reflex.Dom

import Sisyphus.Api
import Sisyphus.ProductManagement.Pbs
import Sisyphus.Reflex.PbsInput


main :: IO ()
main = mainWidgetWithHead elHead elBody

elHead :: (MonadWidget t m) => m ()
elHead = do
    el "title" $ text "Sisyphus Robot"

elBody :: forall t m. (MonadWidget t m) => m ()
elBody = do
    el "div" $ do
        dynPbs <- elLoad
        dyn $ dynPbs `ffor` \(pbs :: Pbs) -> do
            rec elSave pbsDyn'
                pbsIn <- el "table" $ do
                    el "thead" $ el "tr" $ do
                        el "th" $ blank
                        el "th" $ text "Code"
                        el "th" $ text "Name"
                    el "tbody" $ do
                        pbsInput $ PbsInputConfig pbs
                let pbsDyn' = _pbsInput_value pbsIn :: Dynamic t Pbs
            el "div" $ display pbsDyn'
            pure pbsIn
        blank
    where
    elLoad :: (MonadWidget t m) => m (Dynamic t Pbs)
    elLoad = do
        evPostBuild <- getPostBuild
        evLoadBtn <- button "Load from server"
        evLoadRsp <- getThe $ leftmost [evLoadBtn, evPostBuild]
        foldDyn (\n p -> maybe p id n) (freshPbs undefined "New Product") evLoadRsp
    elSave :: (MonadWidget t m) => Dynamic t Pbs -> m (Event t ())
    elSave dynPbs = do
        evSaveBtn <- button "Save to server"
        evSaveOk <- putThe $ tagPromptlyDyn dynPbs evSaveBtn
        -- TODO notify about saved state
        pure evSaveOk





getThe :: (MonadWidget t m) => Event t () -> m (Event t (Maybe Pbs))
getThe ev = do
    rsp <- performRequestAsync $ build_getThe <$ ev
    pure $ from_getThe <$> rsp
    where
    build_getThe :: XhrRequest ()
    build_getThe = XhrRequest "GET" "/the" def
    from_getThe :: XhrResponse -> Maybe Pbs
    from_getThe rsp = decodeText =<< _xhrResponse_responseText rsp

putThe :: (MonadWidget t m) => Event t Pbs -> m (Event t ())
putThe ev = do
    rsp <- performRequestAsync $ build_putThe <$> ev
    pure $ () <$ rsp
    where
    build_putThe :: Pbs -> XhrRequest Text
    build_putThe pbs = postJson "/the" pbs
