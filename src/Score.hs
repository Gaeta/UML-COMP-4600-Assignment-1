module Score where

import Cards

-------------------
--- Section 2.1 ---
-------------------

-- Exercise 2.1
---------------

-- The simplified rules of blackjack scoring assigns a numeric score to each
-- Card based on this formula:
--
--   * Every Ace card has a score of 11
--   * Every Face card has a score of 10
--   * A Numeric card has the same score as the number on the card

-- Implement the `cardValue` function which takes a Card as an argument and
-- returns its numeric score according to the above formula.
cardValue :: Card -> Int
cardValue card = undefined -- **FILL IN HERE**

-- Hint: you can pattern-match on the `card` parameter to see which constructor
-- was used to build it and what were the arguments given to the constructor at
-- the time it was built. This lets you write three different defining equations
-- for the `cardValue` function that returns a different value depending on what
-- the argument looks like.


-- Exercise 2.2
---------------

-- Implement the `handValue` function which calculates the score for an entire
-- hand (which is a list of Cards) by:
--
--   1. calculating the score of each Card individually, and then
--
--   2. summing together the individual numbers from part 1.

handValue :: Hand -> Int
handValue hand = undefined -- **FILL IN HERE**


-------------------------
--- Bonus Section 2.2 ---
-------------------------

-- Bonus Exercise 2.3
---------------------

-- ** FILL IN HERE **
data Score = ScoreNotImplemented -- ** REPLACE WITH YOUR DEFINITION **
  deriving Show

cardScore :: Card -> Score
cardScore card = undefined -- **FILL IN HERE**

scoreValue :: Score -> Int
scoreValue score = undefined -- **FILL IN HERE**

isSoft, isHard :: Score -> Bool
isSoft score = undefined -- **FILL IN HERE**
isHard score = undefined -- **FILL IN HERE**


-- Bonus Exercise 2.4
---------------------

instance Eq Score where
  -- (==) :: Score
  score1 == score2 = undefined -- **FILL IN HERE**

instance Ord Score where
  -- compare :: Score -> Score -> Ordering
  compare score1 score2 = undefined -- **FILL IN HERE**

instance Semigroup Score where
  -- (<>) :: Score -> Score -> Score
  score1 <> score2 = undefined -- **FILL IN HERE**

instance Monoid Score where
  -- mempty :: Score
  mempty = undefined -- **FILL IN HERE**


-- Bonus Exercise 2.5
---------------------

handScore :: Hand -> Score
handScore hand = undefined -- **FILL IN HERE**


-- Bonus Exercise 2.6
---------------------

isBust :: Score -> Bool
isBust score = undefined -- **FILL IN HERE**


-- Bonus Exercise 2.7
---------------------

improveScore :: Score -> Score
improveScore score = undefined -- **FILL IN HERE**

betterHandValue :: Hand -> Int
betterHandValue hand = undefined -- **FILL IN HERE**
