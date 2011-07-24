{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
-- | IndexedSet allows a set to be indexed on arbitrary functions, improving query speed on these attributes at the cost of insertion speed. This is mostly useful for large datasets where querying occurs much more frequently than insertion or other updates.
--
-- Running times for each function are included in the documentation. Unless mentioned otherwise, n is the amount of values in the set and k is the amount of indices. It is assumed that yout indexation functions run in constant time.
module IndexedSet ( queryIndex
                  , queryRangeIndex
                  , rebuildIndex
                  , insertInIndex
                  , deleteFromIndex
                  , mapIndex
                  , filterIndex
                  , deriveMap
                  , deriveFilter
                  , deriveInsert
                  , deriveDelete
                  , si
                  , SetIndex
                  , Cond(..)
                  ) where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Maybe
import           Language.Haskell.TH

-- | Query the set on a given index. Querying on an invalid index (for example one that has been removed) will cause a runtime error. Running time: O(log n)
queryIndex :: SetIndex a b -> b -> [a]
queryIndex (SetIndex _ ix) b = S.toList $ maybeSet $ M.lookup b ix

-- | Query a set index based on some conditions. Useful for querying a range. This is an AND-based query; all conditions must match
queryRangeIndex :: SetIndex a b -> [Cond b] -> [a]
queryRangeIndex (SetIndex _ ix) conds = let allkeys = setIntersections $ map (keysForCond (M.keysSet ix)) conds in
    concatMap (\key -> S.toList $ maybeSet $ M.lookup key ix) (S.toList allkeys)
      where keysForCond :: (Ord a)  => Set a -> Cond a -> Set a
            keysForCond keys (CondEQ a)  = if S.member a keys then S.singleton a else S.empty
            keysForCond keys (CondNE a)  = if S.member a keys then S.empty else S.singleton a
            keysForCond keys (CondGT a)  = snd $ S.split a keys
            keysForCond keys (CondLT a)  = fst $ S.split a keys
            keysForCond keys (CondLTE a) = keysForCond keys (CondLT a) `S.union` keysForCond keys (CondEQ a)
            keysForCond keys (CondGTE a) = keysForCond keys (CondGT a) `S.union` keysForCond keys (CondEQ a)

-- | Update a 'SetIndex' so that it matches the given set
rebuildIndex :: Set a -> SetIndex a b -> SetIndex a b
rebuildIndex newset (SetIndex fn _) = S.fold insertInIndex (SetIndex fn M.empty) newset

-- | Create an empty 'SetIndex' for the given function. Use 'rebuildIndex' to fill the index
si fn = SetIndex fn M.empty

-- | Used for querying an index.
data Cond a = (Ord a) => CondGT a | CondLT a | CondNE a | CondGTE a | CondLTE a | CondEQ a

-- | Insert an item into an index. Running time is O(log n)
insertInIndex item (SetIndex fn mp) = SetIndex fn (M.alter (insOrCons item) (fn item) mp)
  where insOrCons a (Just set) = Just (S.insert a set)
        insOrCons a Nothing = Just (S.singleton a)

-- | Remove an item from the set. Running time: O((k+1) log n)
deleteFromIndex item (SetIndex fn mp) = SetIndex fn (M.alter (return . S.delete item . maybeSet) (fn item) mp)

-- | Remove all items from the set that do not match some function. Running time O(n k log n)
filterIndex :: (a -> Bool) -> SetIndex a b -> SetIndex a b
filterIndex filterfn (SetIndex fn mp) = S.fold deleteFromIndex (SetIndex fn mp) toRemove
  where toRemove = S.filter (not . filterfn) (S.unions $ M.elems mp)

-- | Apply a function to every element in the set. Running time O(n k log n)
mapIndex :: (Ord a, Ord b) => (a -> a) -> SetIndex a b -> SetIndex a b
mapIndex mapfn (SetIndex fn mp) = rebuildIndex newset (SetIndex fn mp) where
  newset = S.map mapfn (S.unions $ M.elems mp)

---------------------------

maybeSet (Just lst) = lst
maybeSet Nothing = S.empty

setIntersections [x] = x
setIntersections (x:xs) = S.intersection x (setIntersections xs)

data SetIndex a b = (Ord a, Ord b) => SetIndex (a -> b) (Map b (Set a))

---- Template haskell ----

derive :: Name -> Name -> Name -> Q Exp
derive datatype ixfunname setfunname = do
  (NormalC ctorname ctorfields) <- (fmap toNormalC $ getCtor datatype)
  fnname <- newName "fn"
  varnames <- newnames (length ctorfields) "ctorvar"
  let paramvars = map VarP varnames
  let setfun fn set = AppE (AppE (VarE setfunname) (VarE fn)) (VarE set)
  let ixfun fn ix = AppE (AppE (VarE ixfunname) (VarE fn)) (VarE ix)
  return $ LamE [VarP fnname, ConP ctorname paramvars] (wrapAppE (ConE ctorname) (applyfuns (setfun fnname) (ixfun fnname) varnames))
    where applyfuns setfun ixfun (set:indices) = setfun set : map ixfun indices

deriveMap a = derive a 'mapIndex 'S.map
deriveFilter a = derive a 'filterIndex 'S.filter
deriveInsert a = derive a 'insertInIndex 'S.insert
deriveDelete a = derive a 'deleteFromIndex 'S.delete

toNormalC (RecC name vst) = NormalC name (map (\(a,b,c) -> (b,c)) vst)
toNormalC (NormalC name vt) = NormalC name vt

-- apply the function toWrap to all args
wrapAppE toWrap args = foldl AppE toWrap args

newnames 0 _ = return []
newnames count prefix = do
  nm <- newName prefix
  othernames <- newnames (count-1) prefix
  return (nm:othernames)

getCtor a = do
  TyConI (DataD _ _ _ [c] _) <- reify a
  return c
