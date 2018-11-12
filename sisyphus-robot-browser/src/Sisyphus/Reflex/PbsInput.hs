module Sisyphus.Reflex.PbsInput where

import Control.Monad
import Reflex.Dom

import Sisyphus.ProductManagement.Pbs


data PbsInput t = PbsInput
    { _pbsInput_value :: Dynamic t Pbs
    }

data PbsInputConfig t = PbsInputConfig
    { _pbsInputConfig_initialValue :: Pbs
    }

pbsInput :: forall t m. (MonadWidget t m) => PbsInputConfig t -> m (PbsInput t)
pbsInput PbsInputConfig{..} = do
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

    let evCommit = () <$ _textInput_input ti
    let evName = tagPromptlyDyn (value ti) evCommit
    let evRecord = PbsRecord initHierCode initFlatCode <$> evName
    dynRecord <- holdDyn _pbsRecordInputConfig_initialValue evRecord
    pure $ PbsRecordInput dynRecord
