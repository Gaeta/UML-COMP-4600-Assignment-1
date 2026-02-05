module Shuffle where

import Cards
import Data.List

-----------------
--- Section 3 ---
-----------------

-- Exercise 3.1
---------------

-- Define the polymorphic data type `Indexed i a` which has one constructor for
-- combining together an item of type `a` with an index of type `i`.  Derive an
-- instance of the Show class for this data type.

-- ** FILL IN HERE **
data Indexed i a = IndexedNotImplemented -- **REPLACE WITH YOUR DEFINITION**
  deriving Show

-- To tell the test suite how to build indexed values, define this top-level
-- function for making a new value of type `Indexed i a` out of any index of
-- type `i` and value of type `a`.
at :: a -> i -> Indexed i a
a `at` i = undefined -- **FILL IN HERE**

-- Define these two functions for accessing the two different parts of an
-- `Indexed i a` value.
item :: Indexed i a -> a
item ix = undefined -- **FILL IN HERE**

index :: Indexed i a -> i
index ix = undefined -- **FILL IN HERE**


-- Exercise 3.2
---------------

-- The purpose of the `Indexed i a` is to have custom comparison methods that
-- differ from the default ones of the plain old pair type `(i, a)`.

-- First, define an instance of the Eq class for `Indexed i a`, under the
-- assumption that `i` is also an `Eq` type, by filling in the right-hand side
-- of the == operator below.
instance Eq i => Eq (Indexed i a) where
  -- (==) :: Eq i => Indexed i a -> Indexed i a -> Bool
  x == y = undefined -- **FILL IN HERE**

-- Second, define an instance of the Ord class for `Indexed i a`, under the
-- assumption that `i` is also an `Ord` type, by filling in the right-hand side
-- of the `compare` function below.
instance Ord i => Ord (Indexed i a) where
  -- compare :: Ord i => Indexed i a -> Indexed i a -> Ordering
  compare x y = undefined -- **FILL IN HERE**

-- Hint: Remember that you can pattern match on any value of a data type to see
-- how it was built (which constructor applied to which arguments).  This is
-- useful when you have parameters like `x` and `y` above that belong to a data
-- type like `Indexed i a`, since you can pull apart the unknown value of `x`
-- and `y` to decide what to do.


-- Exercise 3.3
---------------

-- The `addIndexes` function takes two lists, a list of `i`s and a list of `a`s,
-- and combines them together pairwise into a single list of `Indexed i a`s.  If
-- either list has more elements than the other, the extra elements are dropped.
addIndexes :: [i] -> [a] -> [Indexed i a]
addIndexes ixs as = undefined -- **FILL IN HERE**

-- Hint: You can define combineIndexes by pattern-matching on the two parameters
-- `ixs` and `as` to see what to do.  If either one is the empty list, then the
-- result should be empty.  If they are both a non-empty list (starting with one
-- element and then continuing on with the rest of the list), then build the
-- first `Indexed i a` value out of those two first elements, and add it to a
-- list built from the recursive call to `combineIndexes` on the remainder of
-- the two parameters.
--
-- Alternatively, you may use the function
--
--          zip :: [a] -> [b] -> [(a, b)]
--
-- from the standard library that combines two lists into a list of pairs of
-- elements both drawn from the same position (0, 1, 2, etc.)  of the two lists.
-- After `zip`ping `ixs` and `as` together, consider how to convert the list of
-- pairs into a a list of `Indexed Int a` values using a list comprehension that
-- pattern-matches on each pair of values from `zip`s result.


-- The `removeIndexes` function takes a list of Indexed i a values and strips
-- off the indexes to return just the items of type `a`.
removeIndexes :: [Indexed i a] -> [a]
removeIndexes xs = undefined -- **FILL IN HERE**

-- Hint: You can define this function by pattern-matching on the parameter `xs`
-- and recursing in the case that `xs` matches with the non-empty list `x:xs'`.
-- Alternatively, You can try producing this list via a list comprehension that
-- pattern-matches on each element of type `Indexed i a` element to return the
-- `a` item inside of it at each position.


-- Exercise 3.4
---------------

-- Implement the `shuffle` function which takes a list of integers and a list of
-- elements (of any type `a`) and returns a reordered list of `a`s following the
-- relative order of the given integers.
shuffle :: [Int] -> [a] -> [a]
shuffle ixs as = undefined -- **FILL IN HERE**

-- Hint: You can implement the `shuffle` function by combining the results of
-- these three steps:
--
--   1. Combine together each Int and `a` value, taken pairwise the two list
--      parameters, to make a list of type [Indexed Int a] (as done by
--      `addIndexes`).
--
--   2. Sort the list of type `[Indexed i a]` given as the result of step 1
--      according to the Ord instance for Indexed Int a above.  Note that the
--      standard library function
--
--          sort :: Ord a => [a] -> [a]
--
--      Already implements sorting of any list containing Ord elements.
--
--   3. Return a list of `a`s that is generated by dropping all of the `i`
--      indexes from the list given as the result of step 2 (as done by
--      `removeIndexes`).
