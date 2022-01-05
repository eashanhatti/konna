{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elaboration.Items where

import Elaboration.Effect
import Elaboration.Error
import qualified Elaboration.Term as ET
import Surface as S
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
import Prelude hiding(Ordering, lookup)
import qualified Prelude

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

type ItemList = Map Natural FItem

update :: [FItem] -> [FItem]
update items = elems (loop (fromList $ zip (map unId items) items)) where
  loop :: ItemList -> ItemList
  loop items =
    let
      items' = step items
    in
      if all (uncurry noChange) (map (bimap unInfo unInfo) $ zip (map snd $ toList items) (map snd $ toList items')) then
        items'
      else
        loop items'
  noChange :: ItemInfo -> ItemInfo -> Bool
  noChange (ItemInfo i b d ps) (ItemInfo i' b' d' ps') = i == i' && b && b' && d == d' && keysSet ps == keysSet ps'
  step :: ItemList -> ItemList
  step items = fmap (go items) items
  -- Inspecting B
  go :: ItemList -> FItem -> FItem
  go items item =
    let
      loop :: [(Natural, Set ItemPart)] -> SM.State (Bool, Map ItemPart (C.Term, TermAst)) ()
      loop = \case
        [] -> pure ()
        (iid, dep):deps -> do
          when (member Sig dep)
            case items ! iid of
              (hasChanged Sig -> True) -> SM.modify $ first (|| True)
              (hasChanged Def -> True) -> SM.modify $ bimap (|| True) (delete Sig)
              _ -> pure ()
          when (member Def dep)
            case items ! iid of
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

check :: Elab sig m => [FItem] -> m [(C.Item, FItem)]
check items =
  let
    deps = dependencies items
    items' = update items
  in case ordering deps of
    Prelude.Right ord -> loop items' ord -- (map (\(unItem -> (item, f)) -> (unName item, f item)) items') ord
    Prelude.Left _ -> error "TODO"
  where
    -- TODO: Use a `Map` instead of a `[]` and preserve item ordering on the AST some other way
    loop :: Elab sig m => [FItem] -> Ordering -> m [(C.Item, FItem)]
    loop items = \case
      [] -> pure []
      iids:ord -> do
        let availableItems = filter (flip member iids . unId) items
        sigs <- fromList <$> mapM declare availableItems
        items' <- mapM (define sigs) availableItems
        nextItems <- loop items ord
        pure $ items' ++ nextItems
    declare :: Elab sig m => FItem -> m (Natural, (C.Item, FItem))
    declare item = do
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
        FProdDef (ItemInfo _ False _ (lookup Sig -> Just sig)) (NameAst name) _ ->
          pure (name, fst sig, snd sig)
        FProdDef (ItemInfo _ True _ _) (NameAst name) sig -> do
          (cSig, sig') <- ET.check sig (N.gen N.TypeType1)
          pure (name, cSig, sig')
      undefined
    define :: Elab sig m => Map Natural (C.Item, FItem) -> FItem -> m (C.Item, FItem)
    define = undefined

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