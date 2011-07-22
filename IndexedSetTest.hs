module Main where

import Test.Hspec
import Test.Hspec.HUnit
import Test.HUnit
import Data.Either
import Control.Monad.Instances
import Control.Monad
import Prelude hiding (map, filter, null)
import qualified Prelude as P
import qualified Data.Set as S

import IndexedSet

must_eq actual expected = assertEqual "" expected actual

(intset, intsetindex) = addIndex show $ insertMany [1,2,3,4,5] empty

intplus :: Integer -> Integer
intplus = (1+)
testInsert = describe "indexing" [
  it "should index what is inserted" (
     queryByIndex "1" intsetindex intset `must_eq` S.fromList [1]
     ),
  it "should update index on remove" (
    let newset = delete 2 intset in
      S.null $ queryByIndex "2" intsetindex newset
    ),
  it "should allow adding new indices" (
    let (newset, newix) = addIndex intplus intset in
    queryByIndex (2 :: Integer) newix newset `must_eq` S.fromList [1]
    ),
  it "shouldn't remove old indices" (
    let (newset, newix) = addIndex intplus intset in
    queryByIndex "1" intsetindex intset `must_eq` S.fromList [1]
    ),
  it "should update indices on map" (
    let newset = map (1+) intset in
    (S.null $ queryByIndex "1" intsetindex newset) &&
    (queryByIndex "6" intsetindex newset == S.fromList [6])
    ),
  it "should update indices on filter" (
    let newset = filter even intset in
    (S.null $ queryByIndex "1" intsetindex newset) &&
    (not $ 1 `member` newset) &&
    (2 `member` newset) &&
    (queryByIndex "2" intsetindex newset == S.fromList [2])
    )
  ]

tests = descriptions [testInsert]

main = hspec tests