module Sisyphus.Reflex.PbsInput where

import Control.Arrow
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
    pbsIn <- pbsRecordInput $ PbsRecordInputConfig (_info initPbs)
    let dynRecord = _pbsRecordInput_value pbsIn
    case initPbs of
        Tree _ initSubtrees -> do
            pbsIns <- forM initSubtrees $ \subtree -> do
                pbsInput (PbsInputConfig subtree)
            let dynSubtrees = sequence (_pbsInput_value <$> pbsIns)
                dynPbs = Tree <$> dynRecord <*> dynSubtrees
            pure $ PbsInput dynPbs
        Leaves _ initRecords -> do
            recordIns <- forM initRecords $ \record -> do
                pbsRecordInput (PbsRecordInputConfig record)
            let dynRecords = sequence (_pbsRecordInput_value <$> recordIns)
                dynLeaves = Leaves <$> dynRecord <*> dynRecords
            pure $ PbsInput dynLeaves


data PbsRecordInput t = PbsRecordInput
    { _pbsRecordInput_value :: Dynamic t PbsRecord
    }

data PbsRecordInputConfig t = PbsRecordInputConfig
    { _pbsRecordInputConfig_initialValue :: PbsRecord
    }

pbsRecordInput :: (MonadWidget t m) => PbsRecordInputConfig t -> m (PbsRecordInput t)
pbsRecordInput PbsRecordInputConfig{..} = el "tr" $ do
    let initRecord = _pbsRecordInputConfig_initialValue
    el "td" $ text (displayCode initRecord)
    ti <- el "td" $ do
        textInput def { _textInputConfig_initialValue = name initRecord }

    let evCommit = () <$ _textInput_input ti
    let evName = tagPromptlyDyn (value ti) evCommit
    let evRecord = (\name -> initRecord{name = name}) <$> evName -- FIXME foldDyn ($) to update
    dynRecord <- holdDyn initRecord evRecord
    pure $ PbsRecordInput dynRecord
