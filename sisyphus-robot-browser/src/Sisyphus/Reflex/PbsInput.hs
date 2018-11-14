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
    rec dynPbs <- foldDynMaybe ($) initPbs evUpdate
        dynMajor <- holdDyn initPbs $ tagPromptlyDyn dynPbs evMajor
        evEvUpdate <- dyn $ dynMajor `ffor` \case
            Tree initRecord initSubtrees -> do
                (evMajor, dynRecord) <- el "tr" $ do
                    evMajor <- el "td" $ do
                        evAddAsm <- button "+asm"
                        evAddPart <- if null initSubtrees
                                        then button "+part"
                                        else pure never
                        pure $ leftmost
                            [ addFreshPart <$ evAddPart
                            , addFreshAssembly <$ evAddAsm
                            ]
                    recordIn <- pbsRecordInput $ PbsRecordInputConfig initRecord
                    let dynRecord = _pbsRecordInput_value recordIn
                    pure (evMajor, dynRecord)
                subtreeIns <- forM initSubtrees $ \initSubtree -> do
                    pbsInput (PbsInputConfig initSubtree)
                let dynSubtrees = sequence (_pbsInput_value <$> subtreeIns)
                    dynInner = Tree <$> dynRecord <*> dynSubtrees
                    (evInner :: Event t (Pbs -> Maybe Pbs)) = const . Just <$> updated dynInner
                pure $ leftmost [Left <$> evMajor, Right <$> evInner]
            Leaves initRecord initRecords -> do
                (evMajor, dynRecord) <- el "tr" $ do
                    evMajor <- el "td" $ do
                        evAddPart <- button "+part"
                        pure $ addFreshPart <$ evAddPart
                    recordIn <- pbsRecordInput $ PbsRecordInputConfig initRecord
                    let dynRecord = _pbsRecordInput_value recordIn
                    pure (evMajor, dynRecord)
                recordIns <- forM initRecords $ \initRecord -> do
                    el "tr" $ do
                        el "td" blank
                        pbsRecordInput (PbsRecordInputConfig initRecord)
                let dynRecords = sequence (_pbsRecordInput_value <$> recordIns)
                    dynInner = Leaves <$> dynRecord <*> dynRecords
                    evInner = const . Just <$> updated dynInner
                pure $ leftmost [Left <$> evMajor, Right <$> evInner]


        (evUpdateE :: Event t (Either (Pbs -> Maybe Pbs) (Pbs -> Maybe Pbs))) <- switchHoldPromptly never evEvUpdate
        let evUpdate = evUpdateE `ffor` \case
                Left (major :: Pbs -> Maybe Pbs) -> major
                Right (inner :: Pbs -> Maybe Pbs) -> inner
        let (evMajor, _) = fanEither evUpdateE
    pure $ PbsInput dynPbs


data PbsRecordInput t = PbsRecordInput
    { _pbsRecordInput_value :: Dynamic t PbsRecord
    }

data PbsRecordInputConfig t = PbsRecordInputConfig
    { _pbsRecordInputConfig_initialValue :: PbsRecord
    }

pbsRecordInput :: (MonadWidget t m) => PbsRecordInputConfig t -> m (PbsRecordInput t)
pbsRecordInput PbsRecordInputConfig{..} = do
    el "td" blank
    let initRecord = _pbsRecordInputConfig_initialValue
    el "td" $ text (displayCode initRecord)
    ti <- el "td" $ do
        textInput def { _textInputConfig_initialValue = name initRecord }

    let evCommit = () <$ _textInput_input ti
    let evName = tagPromptlyDyn (value ti) evCommit
    let evRecord = (\name -> initRecord{name = name}) <$> evName -- FIXME foldDyn ($) to update
    dynRecord <- holdDyn initRecord evRecord
    pure $ PbsRecordInput dynRecord
