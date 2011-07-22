{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
-- | IndexedSet allows a set to be indexed on arbitrary functions, improving query speed on these attributes at the cost of insertion speed. This is mostly useful for large datasets where querying occurs much more frequently than insertion or other updates.
--
-- Running times for each function are included in the documentation. Unless mentioned otherwise, n is the amount of values in the set and k is the amount of indices. It is assumed that yout indexation functions run in constant time.
module IndexedSet ( 
  insert,
  insertMany,
  queryByIndex,
  queryRangeByIndex,
  addIndex,
  removeIndex,
  toSet,
  member,
  empty,
  null,
  delete,
  filter,
  map,
  Cond (..)
  ) where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Maybe
import           Data.Typeable
import           Prelude hiding (filter, map, null, EQ, GT, LT)
import qualified Prelude as P

-- | Inserts an item into the set, also updates the indices. Running time: O((k+1) log n)
insert :: (Ord a) => a -> IndexedSet a -> IndexedSet a
insert a (IndexedSet s indices i) = 
  IndexedSet (S.insert a s) (M.map (insertInIndex a) indices) i

-- | Inserts multiple items into the set, and updates the indices. Running time: |lst| * O((k+1) log n).
insertMany :: (Ord a) => [a] -> IndexedSet a -> IndexedSet a
insertMany lst set = foldl (flip insert) set lst

-- | Query the set on a given index. Querying on an invalid index (for example one that has been removed) will cause a runtime error. Running time: O(log n)
queryByIndex :: (Typeable b) => b -> Index -> IndexedSet a -> Set a
queryByIndex b ix (IndexedSet s m i) = case fromJust $ M.lookup ix m of
    SetIndex fn indexMap -> maybeSet $ M.lookup (fromJust $ cast b) indexMap

-- | Query a set index based on some conditions. Useful for querying a range. This is an AND-based query; all conditions must match
queryRangeByIndex :: (Ord b, Typeable b) => [Cond b] -> Index -> IndexedSet a -> [a]
queryRangeByIndex conds ix (IndexedSet s m i) = case fromJust $ M.lookup ix m of
  SetIndex fn indexMap -> let allkeys = setIntersections $ P.map (keysForCond (fromJust $ cast $ M.keysSet indexMap)) conds in
    P.concatMap (\key -> S.toList $ maybeSet $ M.lookup key indexMap) (fromJust $ cast $ S.toList allkeys)
      where keysForCond :: (Ord a, Typeable a)  => Set a -> Cond a -> Set a
            keysForCond keys (EQ a)  = if S.member a keys then S.singleton a else S.empty
            keysForCond keys (NE a)  = if S.member a keys then S.empty else S.singleton a
            keysForCond keys (GT a)  = snd $ S.split (fromJust $ cast a) keys
            keysForCond keys (LT a)  = fst $ S.split a keys
            keysForCond keys (LTE a) = S.union (keysForCond keys (LT a)) (keysForCond keys (EQ a))            
            keysForCond keys (GTE a) = S.union (keysForCond keys (GT a)) (keysForCond keys (EQ a))

setIntersections [x] = x
setIntersections (x:xs) = S.intersection x (setIntersections xs)

-- | Create an empty IndexedSet with no indices. Running time: O(1)
empty :: (Ord a) => IndexedSet a
empty = IndexedSet S.empty M.empty 0

-- | Add an index to the set. Running time: O(n log n + log k)
addIndex :: (Typeable b, Ord b) => 
            (a -> b) -> IndexedSet a -> (IndexedSet a, Index)
addIndex fn (IndexedSet set indices i) = (IndexedSet set newindices (i+1), i)
  where newindices = M.insert i (rebuildIndex set $ SetIndex fn M.empty) indices

-- | Remove an index. Running time: O(log k)
removeIndex :: Index -> IndexedSet a -> IndexedSet a
removeIndex ix (IndexedSet set indices i) = IndexedSet set (M.delete ix indices) i

-- | Get the data as a regular 'Data.Set'. Running time: O(1)
toSet :: IndexedSet a -> Set a
toSet (IndexedSet set _ _) = set

-- | Check if the set contains some object. Running time: O(log n)
member item ixset = S.member item (toSet ixset)

-- | Remove an item from the set. Running time: O((k+1) log n)
delete :: a -> IndexedSet a -> IndexedSet a
delete item (IndexedSet set indices i) = IndexedSet (S.delete item set) (removeFromIndices item indices) i where
  removeFromIndices item indices = M.map (removeFromIndex item) indices
  removeFromIndex item index = case index of
    SetIndex fn mp -> SetIndex fn (M.alter (return . S.delete item . maybeSet) (fn item) mp)

-- | Check whether the set is empty. Running time O(1)
null :: IndexedSet a -> Bool
null = S.null . toSet

-- | Remove all items from the set that do not match some function. Running time O(n k log n)
filter :: (a -> Bool) -> IndexedSet a -> IndexedSet a
filter fn ixset = S.fold (delete) ixset (toRemove ixset)
  where toRemove (IndexedSet set _ _) = S.filter (not . fn) set

-- | Apply a function to every element in the set. Running time O(n k log n)
map :: (Ord a) => (a -> a) -> IndexedSet a -> IndexedSet a
map fn (IndexedSet s m i) = IndexedSet newset (rebuildIndices newset m) i where
  newset = S.map fn s
  rebuildIndices newset ixMap = M.map (rebuildIndex newset) ixMap

-- | Used for querying an index.
data Cond a = forall a. (Ord a, Typeable a) => GT a | LT a | NE a | GTE a | LTE a | EQ a

---- Private functions ----

rebuildIndex newset (SetIndex fn _) = S.fold (insertInIndex) (SetIndex fn M.empty) newset

insertInIndex item ix = case ix of
  SetIndex fn mp -> SetIndex fn (M.alter (insOrCons item) (fn item) mp)
  where insOrCons a (Just set) = Just (S.insert a set)
        insOrCons a Nothing = Just (S.singleton a)

maybeSet (Just lst) = lst
maybeSet Nothing = S.empty

---- Private datatypes ----

data IndexedSet a = (Ord a) => IndexedSet (Set a) (Map Index (SetIndex a)) Index

type Index = Int

data SetIndex a = forall b. (Ord b, Typeable b) => SetIndex (a -> b) (Map b (Set a))
