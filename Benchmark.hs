{-# LANGUAGE TemplateHaskell #-}
module Main where

import IndexedSet
import Criterion.Main
import Criterion.Config
import qualified Criterion.MultiMap as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.DeepSeq

data Person = Person { name :: String
                     , age :: Int
                     , country :: String
                     }  deriving (Eq, Ord, Show)

instance NFData Person where
  rnf (Person n a c) = rnf n `seq` rnf a `seq` rnf c

data Database = Database { persons      :: (Set Person) 
                         , ageindex     :: (SetIndex Person Int) 
                         }

instance Show Database where
  show a = show (persons a)

instance NFData Database where
  rnf (Database s (SetIndex fn mp)) = rnf s `seq` rnf mp

emptypersondb = Database S.empty (si age)

personmap = $(deriveMap ''Database)
personfilter = $(deriveFilter ''Database)
persondelete = $(deriveDelete ''Database)
personinsert = $(deriveInsert ''Database)
personinsertmany = foldl (flip personinsert)

personslist = map (\x -> Person (show x) (x`mod`100) (show x)) [1..50000]

main = do
  let db = personinsertmany emptypersondb personslist
  rnf db `seq` 
    defaultMain [ bench "index query" $ nf benchIndexQuery db
                , bench "filter query" $ nf benchFilterQuery db
                ]

benchIndexQuery db = queryIndex (ageindex db) 20
benchFilterQuery db = S.filter (\x -> age x == 20) (persons db)