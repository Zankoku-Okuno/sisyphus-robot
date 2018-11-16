module Sisyphus.Reflex.PbsInput where

import Data.Monoid
import Control.Arrow
import Control.Monad
import Reflex.Dom

import Sisyphus.ProductManagement.Pbs


data PbsInput t = PbsInput
    { _pbsInput_value :: Dynamic t Pbs
    , _pbsInput_delete :: Event t ()
    }

data PbsInputConfig t = PbsInputConfig
    { _pbsInputConfig_initialValue :: Pbs
    }

pbsInput :: forall t m. (MonadWidget t m) => PbsInputConfig t -> m (PbsInput t)
pbsInput PbsInputConfig{..} = do
    let initPbs = _pbsInputConfig_initialValue
    rec dynPbs <- foldDynMaybe ($) initPbs evUpdate
        dynMajor <- holdDyn initPbs $ tagPromptlyDyn dynPbs evMajor
        evEvPair <- dyn $ dynMajor `ffor` \pbs -> do
            _pbsInputRow pbs
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

type PbsUpdate = Pbs -> Maybe Pbs
_pbsInputRow :: forall t m. (MonadWidget t m) => Pbs -> m (Event t (Either PbsUpdate PbsUpdate), Event t ())
_pbsInputRow initPbs = do
    let initCtx = ctx initPbs
        initChildren = children initPbs
    (evUpdateE, evDelSelf) <- case the initPbs of
        Top{..} -> do
            -- FIXME edit the context up here instead of the record, once Top has no record
            (evAdd, recordIn) <- el "tr" $ do
                evAdd <- el "td" $ do
                    evAddAsm <- addAsmBtn
                    pure $ addFreshAssembly <$ evAddAsm
                recordIn <- pbsRecordInput PbsRecordInputConfig
                    { _pbsRecordInputConfig_initialValue = _record
                    , _pbsRecordInputConfig_context = initCtx
                    }
                pure (evAdd, recordIn)
            subasmIns <- forM initChildren $ \initSubasm -> do
                pbsInput (PbsInputConfig initSubasm)

            let dynRecord = _pbsRecordInput_value recordIn
                dynChildren = sequence ((the <$>) . _pbsInput_value <$> subasmIns)
                evDelIxs = zipWith (<$) [0..] (_pbsInput_delete <$> subasmIns)
                evDelChild = removeByIndex <$> leftmost evDelIxs
            let dynInner = Pbs initCtx <$> (Top <$> dynRecord <*> dynChildren)
                evInner = const . Just <$> updated dynInner
                evMajor = leftmost [evAdd, evDelChild]
                evUpdateE = leftmost [Left <$> evMajor, Right <$> evInner]
            pure (evUpdateE, never)
        Tree{..} -> do
            (evAdd, recordIn, evDelSelf) <- el "tr" $ do
                (evAdd, evDelSelf) <- el "td" $ do
                    evAddAsm <- addAsmBtn
                    evAddPart <- if null _subassemblies
                        then addPartBtn
                        else pure never
                    evDelSelf <- delBtn
                    let evAdd = leftmost [addFreshAssembly <$ evAddAsm, addFreshPart <$ evAddPart]
                    pure (evAdd, evDelSelf)
                recordIn <- pbsRecordInput PbsRecordInputConfig
                    { _pbsRecordInputConfig_initialValue = _record
                    , _pbsRecordInputConfig_context = initCtx
                    }
                pure (evAdd, recordIn, evDelSelf)
            subasmIns <- forM initChildren $ \initSubasm -> do
                pbsInput (PbsInputConfig initSubasm)

            let dynRecord = _pbsRecordInput_value recordIn
                dynChildren = sequence ((the <$>) . _pbsInput_value <$> subasmIns)
                evDelIxs = zipWith (<$) [0..] (_pbsInput_delete <$> subasmIns)
                evDelChild = removeByIndex <$> leftmost evDelIxs
            let dynInner = Pbs initCtx <$> (Tree <$> dynRecord <*> dynChildren)
                evInner = const . Just <$> updated dynInner
                evMajor = leftmost [evAdd, evDelChild]
                evUpdateE = leftmost [Left <$> evMajor, Right <$> evInner]
            pure (evUpdateE, evDelSelf)
        Leaves{..} -> do
            (evAdd, recordIn, evDelSelf) <- el "tr" $ do
                (evAdd, evDelSelf) <- el "td" $ do
                    evAddAsm <- if null _parts
                        then addAsmBtn
                        else pure never
                    evAddPart <- addPartBtn
                    evDelSelf <- delBtn
                    let evAdd = leftmost [addFreshPart <$ evAddPart, addFreshAssembly <$ evAddAsm]
                    pure (evAdd, evDelSelf)
                recordIn <- pbsRecordInput PbsRecordInputConfig
                    { _pbsRecordInputConfig_initialValue = _record
                    , _pbsRecordInputConfig_context = initCtx
                    }
                pure (evAdd, recordIn, evDelSelf)
            partIns <- forM initChildren $ \initPart -> do
                pbsInput (PbsInputConfig initPart)

            let dynRecord = _pbsRecordInput_value recordIn
                dynChildren = sequence ((the <$>) . _pbsInput_value <$> partIns)
                evDelIxs = zipWith (<$) [0..] (_pbsInput_delete <$> partIns)
                evDelChild = removeByIndex <$> leftmost evDelIxs
            let dynInner = Pbs initCtx <$> (Leaves <$> dynRecord <*> dynChildren)
                evInner = const . Just <$> updated dynInner
                evMajor = leftmost [evAdd, evDelChild]
                evUpdateE = leftmost [Left <$> evMajor, Right <$> evInner]
            pure (evUpdateE, evDelSelf)
        Leaf{..} -> el "tr" $ do
            evDelSelf <- el "td" $ delBtn
            recordIn <- pbsRecordInput PbsRecordInputConfig
                { _pbsRecordInputConfig_initialValue = _record
                , _pbsRecordInputConfig_context = initCtx
                }
            let dynRecord = _pbsRecordInput_value recordIn
                dynInner = Pbs initCtx . Leaf <$> dynRecord
                evInner = const . Just <$> updated dynInner
                evUpdateE = Right <$> evInner
            pure (evUpdateE, evDelSelf)
    pure (evUpdateE, evDelSelf)
    where
    addAsmBtn = button "↳" -- U+21b3 Downwards Arrow With Tip Rightwards
    addPartBtn = button "+"
    delBtn = button "🗙" -- U+1F5D9 Cancellation X


data PbsRecordInput t = PbsRecordInput
    { _pbsRecordInput_value :: Dynamic t PbsRecord
    }

data PbsRecordInputConfig t = PbsRecordInputConfig
    { _pbsRecordInputConfig_initialValue :: PbsRecord
    , _pbsRecordInputConfig_context :: PbsCtx
    }

pbsRecordInput :: (MonadWidget t m) => PbsRecordInputConfig t -> m (PbsRecordInput t)
pbsRecordInput PbsRecordInputConfig{..} = do
    let initRecord = _pbsRecordInputConfig_initialValue
        ctx = _pbsRecordInputConfig_context
    el "td" $ text (displayCode ctx)
    ti <- el "td" $ do
        textInput def { _textInputConfig_initialValue = name initRecord }

    let dynName = _textInput_value ti
        dynRecord = PbsRecord <$> dynName
    pure $ PbsRecordInput
        { _pbsRecordInput_value = dynRecord
        }
