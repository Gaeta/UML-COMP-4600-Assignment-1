{-
 * Created By: Donald Gaeta
 * Date: 2/4/26
 *
 * Copyright (c) 2026 Donald J Gaeta (Gaeta.me)
 * This software is provided "as is" for educational and grading purposes only by authorized individuals.
 * Any use, reproduction, distribution, or modification of this software for purposes other than those
 * mentioned is strictly prohibited. Unauthorized use may result in legal action. All rights reserved.
 * The author retains perpetual ownership of this software and its associated materials.
 *
 * Extra Sources Used:
 * - Haskell Documentation (GHC User's Guide, base library docs, Prelude reference)
 * - HaskellWiki (language notes and examples)
 * - Stack Overflow (Haskell tagged questions and answers for troubleshooting and syntax clarification)
-}

module Shuffle where

import Cards
import Data.List

-----------------
--- Section 3 ---
-----------------

-- Exercise 3.1
---------------

data Indexed i a = Indexed a i
    deriving (Show)

at :: a -> i -> Indexed i a
a `at` i = Indexed a i

item :: Indexed i a -> a
item (Indexed a _) = a

index :: Indexed i a -> i
index (Indexed _ i) = i


-- Exercise 3.2
---------------

instance Eq i => Eq (Indexed i a) where
    x == y = index x == index y

instance Ord i => Ord (Indexed i a) where
    compare x y = compare (index x) (index y)


-- Exercise 3.3
---------------

addIndexes :: [i] -> [a] -> [Indexed i a]
addIndexes ixs as = [a `at` i | (i, a) <- zip ixs as]

removeIndexes :: [Indexed i a] -> [a]
removeIndexes xs = map item xs


-- Exercise 3.4
---------------

shuffle :: [Int] -> [a] -> [a]
shuffle ixs as = removeIndexes (sort (addIndexes ixs as))
