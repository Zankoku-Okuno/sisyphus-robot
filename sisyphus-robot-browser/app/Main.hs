module Main where

import Data.Maybe
import Control.Monad
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

elBody :: forall t m. (MonadWidget t m) => m ()
elBody = do
    el "div" $ do
        dynPbs <- elLoad
        elSave dynPbs
        let (<$$>) = flip (<$>)
        dyn $ dynPbs <$$> \(pbs :: Pbs) -> do
            pbsIn <- pbsInput $ PbsInputConfig pbs
            display $ _pbsInput_value pbsIn
            pure pbsIn

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
    elRecord :: (MonadWidget t m) =>
        Dynamic t Pbs ->
        Dynamic t PbsRecord ->
        m (Event t (PbsRecordInput t))
    elRecord dynPbs dynRow =  do
        let dynConfig = PbsRecordInputConfig <$> dynPbs <*> dynRow
        evPbsIn <- dyn $ pbsRecordInput <$> dynConfig
        pure evPbsIn


data PbsInput t = PbsInput
    { _pbsInput_value :: Dynamic t Pbs
    }
data PbsInputConfig t = PbsInputConfig
    { _pbsInputConfig_initialValue :: Pbs
    }
pbsInput :: forall t m. (MonadWidget t m) => PbsInputConfig t -> m (PbsInput t)
pbsInput PbsInputConfig{..} = el "table" $ do
    el "thead" $ el "tr" $ do
        el "th" $ text "Code"
        el "th" $ text "Name"
    el "tbody" $ do
        let initPbs = _pbsInputConfig_initialValue
        rowIns <- forM (records initPbs) $ \row -> do
            pbsRecordInput (PbsRecordInputConfig initPbs row)
        let dynRows = sequence (_pbsRecordInput_value <$> rowIns) :: Dynamic t [PbsRecord]
            dynPbs = Pbs (maxFlatCode initPbs) <$> dynRows :: Dynamic t Pbs
        pure $ PbsInput dynPbs

data PbsRecordInput t = PbsRecordInput
    { _pbsRecordInput_value :: Dynamic t PbsRecord
    }
data PbsRecordInputConfig t = PbsRecordInputConfig
    { _pbsRecordInputConfig_parent :: Pbs
    , _pbsRecordInputConfig_initialValue :: PbsRecord
    }
pbsRecordInput :: (MonadWidget t m) => PbsRecordInputConfig t -> m (PbsRecordInput t)
pbsRecordInput PbsRecordInputConfig{..} = el "tr" $ do
    let initHierCode = hierCode _pbsRecordInputConfig_initialValue
    let initFlatCode = flatCode _pbsRecordInputConfig_initialValue
    el "td" $ text (displayCode _pbsRecordInputConfig_parent _pbsRecordInputConfig_initialValue)
    let initName = name _pbsRecordInputConfig_initialValue
    ti <- el "td" $ do
        textInput def { _textInputConfig_initialValue = initName }

    let evCommit = keypress Enter ti
    let evName = tagPromptlyDyn (value ti) evCommit
    let evRecord = PbsRecord initHierCode initFlatCode <$> evName
    dynRecord <- holdDyn _pbsRecordInputConfig_initialValue evRecord
    pure $ PbsRecordInput dynRecord




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
