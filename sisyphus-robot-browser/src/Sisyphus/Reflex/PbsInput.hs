module Sisyphus.Reflex.PbsInput where

import Data.Monoid
import Control.Arrow
import Control.Monad
import Reflex.Dom

import Sisyphus.ProductManagement.Pbs


data PbsInput t = PbsInput
    { _pbsInput_value :: Dynamic t Pbs
    , _pbsInput_delete :: Event t Code
    }

data PbsInputConfig t = PbsInputConfig
    { _pbsInputConfig_initialValue :: Pbs
    }

pbsInput :: forall t m. (MonadWidget t m) => PbsInputConfig t -> m (PbsInput t)
pbsInput PbsInputConfig{..} = do
    let initPbs = _pbsInputConfig_initialValue
    rec dynPbs <- foldDynMaybe ($) initPbs evUpdate
        dynMajor <- holdDyn initPbs $ tagPromptlyDyn dynPbs evMajor
        evEvPair <- dyn $ dynMajor `ffor` \case
            Tree initRecord initSubtrees -> do
                (evAdd, evDel, dynRecord) <- el "tr" $ do
                    evAdd <- el "td" $ do
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
                        evDel = _pbsRecordInput_delete recordIn
                    pure (evAdd, evDel, dynRecord)
                subtreeIns <- forM initSubtrees $ \initSubtree -> do
                    pbsInput (PbsInputConfig initSubtree)
                let dynSubtrees = sequence (_pbsInput_value <$> subtreeIns)
                    dynInner = Tree <$> dynRecord <*> dynSubtrees
                    evInner = const . Just <$> updated dynInner
                let evDelCodes = _pbsInput_delete <$> subtreeIns
                    evDels = (removeByCode <$>) <$> evDelCodes
                let evMajor = leftmost $ evDels ++ [evAdd] -- FIXME leftmost? really?
                pure $ (leftmost [Left <$> evMajor, Right <$> evInner], evDel)
            Leaves initRecord initRecords -> do
                (evAdd, evDel, dynRecord) <- el "tr" $ do
                    evAdd <- el "td" $ do
                        evAddPart <- button "+part"
                        pure $ addFreshPart <$ evAddPart
                    recordIn <- pbsRecordInput $ PbsRecordInputConfig initRecord
                    let dynRecord = _pbsRecordInput_value recordIn
                        evDel = _pbsRecordInput_delete recordIn
                    pure (evAdd, evDel, dynRecord)
                recordIns <- forM initRecords $ \initRecord -> do
                    el "tr" $ do
                        el "td" blank
                        pbsRecordInput (PbsRecordInputConfig initRecord)
                let dynRecords = sequence (_pbsRecordInput_value <$> recordIns)
                    dynInner = Leaves <$> dynRecord <*> dynRecords
                    evInner = const . Just <$> updated dynInner
                let evDelCodes = _pbsRecordInput_delete <$> recordIns
                    evDels = (removeByCode <$>) <$> evDelCodes
                let evMajor = leftmost $ evDels ++ [evAdd] -- FIXME leftmost? really?
                pure $ (leftmost [Left <$> evMajor, Right <$> evInner], evDel)
        let (evEvUpdateE, evEvDel) = splitE evEvPair
        evUpdateE <- switchHoldPromptly never evEvUpdateE
        evDel <- switchHoldPromptly never evEvDel
        let evUpdate = evUpdateE `ffor` \case
                Left major -> major
                Right inner -> inner
        let (evMajor, _) = fanEither evUpdateE
    pure $ PbsInput
        { _pbsInput_value = dynPbs
        , _pbsInput_delete = evDel
        }


data PbsRecordInput t = PbsRecordInput
    { _pbsRecordInput_value :: Dynamic t PbsRecord
    , _pbsRecordInput_delete :: Event t Code
    }

data PbsRecordInputConfig t = PbsRecordInputConfig
    { _pbsRecordInputConfig_initialValue :: PbsRecord
    }

pbsRecordInput :: (MonadWidget t m) => PbsRecordInputConfig t -> m (PbsRecordInput t)
pbsRecordInput PbsRecordInputConfig{..} = do
    let initRecord = _pbsRecordInputConfig_initialValue
    evDeleteSelf <- el "td" $ do
        if code initRecord == ([], Nothing)
        then pure never
        else do
            evClick <- button "delete"
            pure $ code initRecord <$ evClick
    el "td" $ text (displayCode initRecord)
    ti <- el "td" $ do
        textInput def { _textInputConfig_initialValue = name initRecord }

    let evCommit = () <$ _textInput_input ti
    let evName = tagPromptlyDyn (value ti) evCommit
    let evRecord = (\name -> initRecord{name = name}) <$> evName -- FIXME foldDyn ($) to update
    dynRecord <- holdDyn initRecord evRecord
    pure $ PbsRecordInput
        { _pbsRecordInput_value = dynRecord
        , _pbsRecordInput_delete = evDeleteSelf
        }
