{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Elaboration.Items where

import Elaboration.Effect
import Elaboration.Error
import {-# SOURCE #-} qualified Elaboration.Term as ET
import Surface as S hiding(unName)
import qualified Core as C
import qualified Norm as N
import qualified Control.Monad.State as SM
import Control.Monad(when)
import Data.Map(Map, notMember, (!), delete, fromList, toList, elems, keysSet, lookup)
import qualified Data.Map as Map
import Data.Set(Set, member, isSubsetOf, union, difference)
import qualified Data.Set as Set
import Data.Bifunctor
import Numeric.Natural
import Data.Data(Data)
import Prelude hiding(Ordering, lookup)
import qualified Prelude
import Etc
import Debug.Trace
import Data.Maybe(fromJust)

-- Given twp items A and B, and B depends on A
-- "Has changed" vs "must be rechecked"
-- A's sig has changed, B's sig is dependent. B's sig has not changed and must be rechecked
-- A's def has changed, B's sig is dependent. B's sig has changed and must be rechecked
-- A's sig has changed, B's def is dependent. B's def has not changed and must be rechecked
-- A's def has changed, B's def is dependent. B's def has changed and must not be rechecked

data FItem
  = FTermDef ItemInfo NameAst TermAst TermAst
  | FIndDef ItemInfo NameAst TermAst
  | FProdDef ItemInfo NameAst TermAst
  | FConDef ItemInfo NameAst TermAst
  deriving Show

type ItemList = [(Natural, FItem)]

data ItemInfo = ItemInfo Natural Bool (Map Natural (Set ItemPart)) (Map ItemPart (C.Term, TermAst))
  deriving (Show, Data)

flatten :: [ItemAst] -> Natural -> ([FItem], SM.State [FItem] [ItemAst])
flatten items iid = case items of
  [] -> ([], pure [])
  (unItem -> (TermDef (ds, ps) name sig def, rebuildItem)):is -> (FTermDef (ItemInfo iid shouldRecheck ds ps) name sig def : flatItems, rebuild) where
    shouldRecheck = not $ member Sig (keysSet ps) && member Def (keysSet ps)
    (flatItems, rebuildItems) = flatten is (iid + 1)
    rebuild = pop >>= \case
      FTermDef (ItemInfo _ _ ds ps) name sig def -> (rebuildItem (TermDef (ds, ps) name sig def) :) <$> rebuildItems
  (unItem -> (IndDef (ds, ps) name sig constrs, rebuildItem)):is -> (FIndDef (ItemInfo iid shouldRecheck ds ps) name sig : flatConstrs ++ flatItems, rebuild) where
    shouldRecheck = not $ member Sig (keysSet ps) && member Def (keysSet ps)
    (flatItems, rebuildItems) = flatten is (iid + fromIntegral (length constrs) + 1)
    (flatConstrs, rebuildConstrs) = flattenConstrs constrs (iid + 1)
    rebuild = pop >>= \case
      FIndDef (ItemInfo _ _ ds ps) name sig -> do
        constrs <- rebuildConstrs
        (rebuildItem (IndDef (ds, ps) name sig constrs) :) <$> rebuildItems

flattenConstrs :: [ConstructorAst] -> Natural -> ([FItem], SM.State [FItem] [ConstructorAst])
flattenConstrs constrs iid = case constrs of
  [] -> ([], pure [])
  (unConstructor -> (Constructor (ds, ps) name sig, rebuildConstr)):cs -> (FConDef (ItemInfo iid shouldRecheck ds ps) name sig : flatConstrs, rebuild) where
    shouldRecheck = not $ member Sig (keysSet ps) && member Def (keysSet ps)
    (flatConstrs, rebuildConstrs) = flattenConstrs cs (iid + 1)
    rebuild = pop >>= \case
      FConDef (ItemInfo _ _ ds ps) name sig -> (rebuildConstr (Constructor (ds, ps) name sig) :) <$> rebuildConstrs

pop = do
  is <- SM.get
  SM.put (tail is)
  pure (head is)

update :: [FItem] -> [FItem]
update items = map snd (loop (zip (map unId items) items)) where
  loop :: ItemList -> ItemList
  loop items =
    let
      items' = step items
    in
      if all (uncurry noChange) (map (bimap unInfo unInfo) $ traceShowId $ zip (map snd items) (map snd items')) then
        items'
      else
        loop items'
  noChange :: ItemInfo -> ItemInfo -> Bool
  noChange (ItemInfo i b d ps) (ItemInfo i' b' d' ps') = i == i' && b == b' && d == d' && keysSet ps == keysSet ps'
  step :: ItemList -> ItemList
  step items = map (second $ go items) items
  -- Inspecting B
  go :: ItemList -> FItem -> FItem
  go items item =
    let
      loop :: [(Natural, Set ItemPart)] -> SM.State (Bool, Map ItemPart (C.Term, TermAst)) ()
      loop = \case
        [] -> pure ()
        (iid, dep):deps -> do
          when (member Sig dep)
            case fromJust $ Prelude.lookup iid items of
              (hasChanged Sig -> True) -> SM.modify $ first (|| True)
              (hasChanged Def -> True) -> SM.modify $ bimap (|| True) (delete Sig)
              _ -> pure ()
          when (member Def dep)
            case fromJust $ Prelude.lookup iid items of
              (hasChanged Sig -> True) -> SM.modify $ first (|| True)
              (hasChanged Def -> True) -> SM.modify $ second (delete Def)
              _ -> pure ()
          loop deps
      (shouldRecheck, oldParts) = SM.execState (loop (toList $ unDependencies item)) (unShouldRecheck item, unOldParts item)
    in withShouldRecheck shouldRecheck . withOldParts oldParts $ item
  hasChanged :: ItemPart -> FItem -> Bool
  hasChanged part item = notMember part (unOldParts item)

type Graph = Map Natural (Map Natural (Set S.ItemPart))
type Cycles = [(Natural, [Natural])]
type Ordering = [Set Natural]

cycles :: Graph -> Cycles
cycles _ = [] -- TODO

ordering :: Graph -> Either Cycles Ordering
ordering graph = case cycles graph of
  [] -> Prelude.Right $ loop (Map.map (Map.keysSet . Map.filter (member Sig)) graph) mempty
  cs -> Prelude.Left cs
  where
    loop :: Map Natural (Set Natural) -> Set Natural -> Ordering
    loop graph available = -- traceShow available $
      if available == Map.keysSet graph then
        []
      else
        let nowAvailable =  Map.keysSet $ Map.filter (\ds -> ds `isSubsetOf` available) graph
        in (nowAvailable `difference` available):(loop graph (nowAvailable `union` available))

dependencies :: [FItem] -> Graph
dependencies = fromList . map go where
  go :: FItem -> (Natural, Map Natural (Set S.ItemPart))
  go item = (unId item, unDependencies item)

check :: Elab sig m => [ItemAst] -> m [(C.Item, ItemAst)]
check items =
  let
    (flatItems, rebuildItems) = flatten items 0
    deps = dependencies flatItems
    flatItems' = update flatItems
  in case ordering deps of
    Prelude.Right ord -> do
      (cItems, flatItems'') <- unzip <$> loop flatItems' ord
      let items' = SM.evalState rebuildItems flatItems''
      pure $ zip cItems items'
    Prelude.Left _ -> error "TODO"
  where
    -- TODO: Use a `Map` instead of a `[]` and preserve item ordering on the AST some other way
    loop :: Elab sig m => [FItem] -> Ordering -> m [(C.Item, FItem)]
    loop items = \case
      [] -> pure []
      iids:ord -> do
        let availableItems = filter (flip member iids . unId) items
        sigs <- mapM checkSig availableItems
        declare (map (bimap fst fst) sigs) $ define (fromList $ map (first snd) sigs) availableItems \items' -> do
          nextItems <- loop items ord
          pure $ items' ++ nextItems
    declare :: Elab sig m => [(Name, C.Term)] -> m [(C.Item, FItem)] -> m [(C.Item, FItem)]
    declare sigs act = case sigs of
      [] -> act
      (name, sig):sigs -> bindGlobal name sig (declare sigs act)
    define :: Elab sig m => Map Natural (C.Term, TermAst) -> [FItem] -> ([(C.Item, FItem)] -> m [(C.Item, FItem)]) -> m [(C.Item, FItem)]
    define sigs items act = go items [] where
      go items acc = case items of
        [] -> act acc
        item:items -> do
          let (cSig, sig) = sigs ! unId item
          (itemDef, item') <- case item of
            FTermDef info name _ def -> do
              vSig <- eval cSig
              (cDef, def') <- ET.check def vSig
              pure (DTermDef cDef, FTermDef info name sig def')
            FIndDef info name sig ->
              pure (DIndDef, FIndDef info name sig)
            FConDef info name sig ->
              pure (DConDef, FConDef info name sig)
          defineGlobal (unName item) itemDef \cItem -> go items ((cItem, item'):acc)
    checkSig :: Elab sig m => FItem -> m ((Name, Natural), (C.Term, TermAst))
    checkSig item = do
      (name, cSig, sig) <- case item of
        FTermDef (ItemInfo _ False _ (lookup Sig -> Just sig)) (NameAst name) _ _ ->
          pure (name, fst sig, snd sig)
        FTermDef (ItemInfo _ True _ _) (NameAst name) sig _ -> do
          meta <- freshUnivMeta
          (cSig, sig') <- ET.check sig meta
          pure (name, cSig, sig')
        FIndDef (ItemInfo _ False _ (lookup Sig -> Just sig)) (NameAst name) _ ->
          pure (name, fst sig, snd sig)
        FIndDef (ItemInfo _ True _ _) (NameAst name) sig -> do
          (cSig, sig') <- ET.check sig (N.gen N.TypeType1)
          pure (name, cSig, sig')
        FConDef (ItemInfo _ False _ (lookup Sig -> Just sig)) (NameAst name) _ ->
          pure (name, fst sig, snd sig)
        FConDef (ItemInfo _ True _ _) (NameAst name) sig -> do -- TODO: Check for `(x₀ : A₀) -> .. -> (xₙ : Aₙ) -> D` form
          meta <- freshUnivMeta
          (cSig, sig') <- ET.check sig meta
          pure (name, cSig, sig')
        FProdDef (ItemInfo _ False _ (lookup Sig -> Just sig)) (NameAst name) _ ->
          pure (name, fst sig, snd sig)
        FProdDef (ItemInfo _ True _ _) (NameAst name) sig -> do
          (cSig, sig') <- ET.check sig (N.gen N.TypeType1)
          pure (name, cSig, sig')
      pure ((name, unId item), (cSig, sig))

-- TODO: Clean up
unId :: FItem -> Natural
unId = \case
  FTermDef (ItemInfo i _ _ _) _ _ _ -> i
  FIndDef (ItemInfo i _ _ _) _ _ -> i
  FProdDef (ItemInfo i _ _ _) _ _ -> i
  FConDef (ItemInfo i _ _ _) _ _ -> i
unDependencies :: FItem -> Map Natural (Set ItemPart)
unDependencies = \case
  FTermDef (ItemInfo _ _ d _) _ _ _ -> d
  FIndDef (ItemInfo _ _ d _) _ _ -> d
  FProdDef (ItemInfo _ _ d _) _ _ -> d
  FConDef (ItemInfo _ _ d _) _ _ -> d
unShouldRecheck :: FItem -> Bool
unShouldRecheck = \case
  FTermDef (ItemInfo _ b _ _) _ _ _ -> b
  FIndDef (ItemInfo _ b _ _) _ _ -> b
  FProdDef (ItemInfo _ b _ _) _ _ -> b
  FConDef (ItemInfo _ b _ _) _ _ -> b
unOldParts :: FItem -> Map ItemPart (C.Term, TermAst)
unOldParts = \case
  FTermDef (ItemInfo _ _ _ ps) _ _ _ -> ps
  FIndDef (ItemInfo _ _ _ ps) _ _ -> ps
  FProdDef (ItemInfo _ _ _ ps) _ _ -> ps
  FConDef (ItemInfo _ _ _ ps) _ _ -> ps
unName :: FItem -> Name
unName = \case
  FTermDef _ (NameAst n) _ _ -> n
  FIndDef _ (NameAst n) _ -> n
  FProdDef _ (NameAst n) _ -> n
  FConDef _ (NameAst n) _ -> n
unInfo :: FItem -> ItemInfo
unInfo = \case
  FTermDef i _ _ _ -> i
  FIndDef i _ _ -> i
  FProdDef i _ _ -> i
  FConDef i _ _ -> i
withOldParts :: Map ItemPart (C.Term, TermAst) -> FItem -> FItem
withOldParts ps = \case
  FTermDef (ItemInfo i b d _) n s e -> FTermDef (ItemInfo i b d ps) n s e
  FIndDef (ItemInfo i b d _) n s -> FIndDef (ItemInfo i b d ps) n s
  FProdDef (ItemInfo i b d _) n s -> FProdDef (ItemInfo i b d ps) n s
withShouldRecheck :: Bool -> FItem -> FItem
withShouldRecheck b = \case
  FTermDef (ItemInfo i _ d ps) n s e -> FTermDef (ItemInfo i b d ps) n s e
  FIndDef (ItemInfo i _ d ps) n s -> FIndDef (ItemInfo i b d ps) n s
  FProdDef (ItemInfo i _ d ps) n s -> FProdDef (ItemInfo i b d ps) n s