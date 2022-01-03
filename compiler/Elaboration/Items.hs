{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Elaboration.Items where

import Elaboration.Effect
import Elaboration.Error
import Surface as S
import qualified Core as C
import qualified Control.Monad.State as SM
import Control.Monad(when)
import Data.Map(Map, notMember, (!), delete, fromList, toList, elems, keysSet)
import qualified Data.Map as Map
import Data.Set(Set, member, difference, union, isSubsetOf)
import Data.Bifunctor
import Prelude hiding(Ordering)

-- Given twp items A and B, and B depends on A
-- "Has changed" vs "must be rechecked"
-- A's sig has changed, B's sig is dependent. B's sig has not changed and must be rechecked
-- A's def has changed, B's sig is dependent. B's sig has changed and must be rechecked
-- A's sig has changed, B's def is dependent. B's def has not changed and must be rechecked
-- A's def has changed, B's def is dependent. B's def has changed and must not be rechecked

type ItemList = Map Name ItemAst

update :: [ItemAst] -> [ItemAst]
update items = elems (loop (fromList $ zip (map (unName . fst . unItem) items) items)) where
  loop :: ItemList -> ItemList
  loop items =
    let
      items' = step items
      unInfo = unItemInfo . fst . unItem
    in
      if all (uncurry noChange) (map (bimap unInfo unInfo) $ zip (map snd $ toList items) (map snd $ toList items')) then
        items'
      else
        loop items'
  noChange :: ItemInfo -> ItemInfo -> Bool
  noChange (ItemInfo b d ps) (ItemInfo b' d' ps') = b && b' && d == d' && keysSet ps == keysSet ps'
  step :: ItemList -> ItemList
  step items = fmap (go items) items
  -- Inspecting B
  go :: ItemList -> ItemAst -> ItemAst
  go items (unItem -> (item, f)) =
    let
      loop :: [(Name, Set ItemPart)] -> SM.State (Bool, Map ItemPart C.Term) ()
      loop = \case
        [] -> pure ()
        (name, dep):deps -> do
          when (member Sig dep)
            case fst $ unItem $ items ! name of
              (hasChanged Sig -> True) -> SM.modify $ first (|| True)
              (hasChanged Def -> True) -> SM.modify $ bimap (|| True) (delete Sig)
              _ -> pure ()
          when (member Def dep)
            case fst $ unItem $ items ! name of
              (hasChanged Sig -> True) -> SM.modify $ first (|| True)
              (hasChanged Def -> True) -> SM.modify $ second (delete Def)
              _ -> pure ()
          loop deps
      (shouldRecheck, oldParts) = SM.execState (loop (toList $ unDependencies item)) (unShouldRecheck item, unOldParts item)
    in f $ withShouldRecheck shouldRecheck . withOldParts oldParts $ item
  hasChanged :: ItemPart -> Item -> Bool
  hasChanged part item = notMember part (S.unOldParts item)

type Graph = Map Name (Map Name (Set ItemPart))
type Cycles = [(Name, [Name])]
type Ordering = [Set Name]

cycles :: Graph -> Cycles
cycles _ = [] -- TODO

ordering :: Graph -> Either Cycles Ordering
ordering graph = case cycles graph of
  [] -> Prelude.Right $ loop (Map.map (Map.keysSet . Map.filter (member Sig)) graph) mempty
  cs -> Prelude.Left cs
  where
    loop :: Map Name (Set Name) -> Set Name -> Ordering
    loop graph available =
      if available == Map.keysSet graph then
        []
      else
        let nowAvailable =  Map.keysSet $ Map.filter (\ds -> ds `isSubsetOf` available) graph
        in (nowAvailable `difference` available):(loop graph (nowAvailable `union` available))