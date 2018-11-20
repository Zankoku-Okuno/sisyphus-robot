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
    , _pbsInputConfig_shown :: Dynamic t Bool
    }

pbsInput :: forall t m. (MonadWidget t m) => PbsInputConfig t -> m (PbsInput t)
pbsInput PbsInputConfig{..} = do
    let initPbs = _pbsInputConfig_initialValue
    rec dynPbs <- foldDynMaybe ($) initPbs evUpdate
        dynMajor <- holdDyn initPbs $ tagPromptlyDyn dynPbs evMajor
        evEvPair <- dyn $ dynMajor `ffor` \pbs -> do
            _pbsInputRow pbs _pbsInputConfig_shown
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
_pbsInputRow :: forall t m. (MonadWidget t m) => Pbs -> Dynamic t Bool -> m (Event t (Either PbsUpdate PbsUpdate), Event t ())
_pbsInputRow initPbs dynShown = do
    let initCtx = ctx initPbs
        initChildren = children initPbs
    (evUpdateE, evDelSelf) <- case the initPbs of
        Top{..} -> do
            -- FIXME edit the context up here instead of the record, once Top has no record
            (evAdd, dynRecord, dynShowKids, evDelSelf) <- row initPbs dynShown
            subasmIns <- forM initChildren $ \initSubasm -> do
                pbsInput (PbsInputConfig initSubasm dynShowKids)

            let dynChildren = sequence ((the <$>) . _pbsInput_value <$> subasmIns)
                evDelIxs = zipWith (<$) [0..] (_pbsInput_delete <$> subasmIns)
                evDelChild = removeByIndex <$> leftmost evDelIxs
            let dynInner = Pbs initCtx <$> (Top <$> dynRecord <*> dynChildren)
                evInner = const . Just <$> updated dynInner
                evMajor = leftmost [evAdd, evDelChild]
                evUpdateE = leftmost [Left <$> evMajor, Right <$> evInner]
            pure (evUpdateE, never)
        Tree{..} -> do
            (evAdd, dynRecord, dynShowKids, evDelSelf) <- row initPbs dynShown
            subasmIns <- forM initChildren $ \initSubasm -> do
                pbsInput (PbsInputConfig initSubasm dynShowKids)

            let dynChildren = sequence ((the <$>) . _pbsInput_value <$> subasmIns)
                evDelIxs = zipWith (<$) [0..] (_pbsInput_delete <$> subasmIns)
                evDelChild = removeByIndex <$> leftmost evDelIxs
            let dynInner = Pbs initCtx <$> (Tree <$> dynRecord <*> dynChildren)
                evInner = const . Just <$> updated dynInner
                evMajor = leftmost [evAdd, evDelChild]
                evUpdateE = leftmost [Left <$> evMajor, Right <$> evInner]
            pure (evUpdateE, evDelSelf)
        Leaves{..} -> do
            (evAdd, dynRecord, dynShowKids, evDelSelf) <- row initPbs dynShown
            partIns <- forM initChildren $ \initPart -> do
                pbsInput (PbsInputConfig initPart dynShowKids)

            let dynChildren = sequence ((the <$>) . _pbsInput_value <$> partIns)
                evDelIxs = zipWith (<$) [0..] (_pbsInput_delete <$> partIns)
                evDelChild = removeByIndex <$> leftmost evDelIxs
            let dynInner = Pbs initCtx <$> (Leaves <$> dynRecord <*> dynChildren)
                evInner = const . Just <$> updated dynInner
                evMajor = leftmost [evAdd, evDelChild]
                evUpdateE = leftmost [Left <$> evMajor, Right <$> evInner]
            pure (evUpdateE, evDelSelf)
        Leaf{..} -> do
            (evAdd, dynRecord, dynShowKids, evDelSelf) <- row initPbs dynShown
            let dynInner = Pbs initCtx . Leaf <$> dynRecord
                evInner = const . Just <$> updated dynInner
                evUpdateE = Right <$> evInner
            pure (evUpdateE, evDelSelf)
    pure (evUpdateE, evDelSelf)
    where
    addAsmBtn = button "↳" -- U+21b3 Downwards Arrow With Tip Rightwards
    addPartBtn = button "+"
    delBtn = button "🗙" -- U+1F5D9 Cancellation X
    visibleBtn = do
        rec evVisibility <- do
                (btn, _) <- element "button" def $ dynText (shownSymbol <$> dynShown)
                pure $ domEvent Click btn
            dynShown <- toggle True evVisibility
        pure dynShown
        where
        shownSymbol True = "⏷" -- U+23F7 Black Medium Down-Pointing Triangle
        shownSymbol False = "⏵" -- U+23F5 Black Medium Right-Pointing Triangle
    row :: Pbs -> Dynamic t Bool -> m (Event t PbsUpdate, Dynamic t PbsRecord, Dynamic t Bool, Event t ())
    row pbs dynShown = elDynAttr "tr" dynAttrs $ do
        (evAdd, dynShowKids, evDelSelf) <- buttons pbs
        recordIn <- pbsRecordInput PbsRecordInputConfig
            { _pbsRecordInputConfig_initialValue = _record the
            , _pbsRecordInputConfig_context = ctx
            }
        let dynRecord = _pbsRecordInput_value recordIn
        pure (evAdd, dynRecord, ((&&) <$> dynShown <*> dynShowKids), evDelSelf)
        where
        Pbs{..} = pbs
        dynAttrs = mkStyle <$> dynShown
            where
            mkStyle True = mempty
            mkStyle False = "style" =: "display:none"

    buttons :: Pbs -> m (Event t PbsUpdate, Dynamic t Bool, Event t ())
    buttons pbs = el "td" $ do
        evDelSelf <- if canDelSelf then delBtn else pure never
        dynShowKids <- if canHide then visibleBtn else pure $ constDyn True
        evAddAsm <- if canAddAsm then addAsmBtn else pure never
        evAddPart <- if canAddPart then addPartBtn else pure never
        let evAdd = leftmost [addFreshAssembly <$ evAddAsm, addFreshPart <$ evAddPart]
        pure (evAdd, dynShowKids, evDelSelf)
        where
        Pbs{..} = pbs
        canAddAsm = case the of
            Leaf{} -> False
            Leaves{_parts = (_:_)} -> False
            _ -> True
        canAddPart = case the of
            Leaves{} -> True
            Tree{_subassemblies = []} -> True
            _ -> False
        canDelSelf = case the of
            Top{} -> False
            _ -> True
        canHide = case the of
            Leaf{} -> False
            _ -> True


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
