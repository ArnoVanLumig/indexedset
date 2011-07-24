{-# LANGUAGE TemplateHaskell #-}
module Main where

import IndexedSet
import Data.Set (Set)
import qualified Data.Set as S
import System.IO

-- This is an example usage of IndexedSet to show the basic functionality. The application will maintain a database of persons, who have a name, age and country. We'll start by defining our datatype Person:

data Person = Person { name :: String
                     , age :: Int
                     , country :: String
                     }  deriving (Eq, Ord, Show)

-- Now we create a datatype that holds a set of persons (note: This must be a set. Lists or other container types will not work), and indices for all three attributes of person. This datatype _must_ have a constructor that looks like this one, starting with the set of items and then one or more indices on that set if you want to use template haskell to create the functions.
data Database = Database { persons   :: (Set Person) 
                         , ageindex  :: (SetIndex Person Int) 
                         , nameindex :: (SetIndex Person String)
                         , countryindex :: (SetIndex Person String)
                         }

-- A simple show instance for our database, that will just output the set of persons.
instance Show Database where
  show a = show (persons a)

-- Create an empty database with the indices. 'si' creates an empty index for a certain function. If you're creating an index for a non-empty set you must call rebuildIndex on that index before using it. Similarly, you should _never_ update the set without updating the indices. Doing this will result in indices that are not consistent with the set.
emptypersondb = Database S.empty (si age) (si name) (si country)

-- Derive the operations on database using template haskell.
personmap = $(deriveMap ''Database)
personfilter = $(deriveFilter ''Database)
persondelete = $(deriveDelete ''Database)
personinsert = $(deriveInsert ''Database)

main = run emptypersondb

run db = do
  putStr "> "; hFlush stdout
  command <- getLine
  case words command of
    ["insert", name, age, country] -> do
      let person = Person name (read age) country
      run (personinsert person db)
    ["list"] -> do
      print db
      run db
    ["queryAge", age] -> do
      print $ queryIndex (ageindex db) (read age)
      run db
    ["queryName", name] -> do
      print $ queryIndex (nameindex db) name
      run db
    ["queryAgeBetween", start, end] -> do
      print $ queryRangeIndex (ageindex db) [CondGT (read start), CondLT (read end)]
      run db
    ["incrementAge"] -> do
      run $ personmap (\p -> p {age = (age p) + 1}) db
    ["delete", name, age, country] -> do
      run $ persondelete (Person name (read age) country) db
    a -> do
      putStrLn $ "unknown command: " ++ (show a)
      run db
