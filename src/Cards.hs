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

module Cards where

-----------------
--- Section 1 ---
-----------------

-- Exercises 1.1 and 1.2
------------------------

data Royalty = Jack | Queen | King
  deriving (Eq, Enum, Bounded)


data Suit = Hearts | Diamonds | Clubs | Spades
    deriving (Eq, Enum, Bounded)
    
hearts, diamonds, clubs, spades :: Suit
hearts   = Hearts
diamonds = Diamonds
clubs    = Clubs
spades   = Spades

data Card
    = Ace Suit
    | Numeric Int Suit
    | Face Royalty Suit
    deriving (Eq)

ace :: Suit -> Card
ace = Ace

numeric :: Int -> Suit -> Card
numeric n suit
    | n >= 2 && n <= 10 = Numeric n suit
    | otherwise = error "numeric: card value must be between 2 and 10"

face :: Royalty -> Suit -> Card
face = Face


-- Exercise 1.3
---------------

type Hand = [Card]

type Deck = [Card]


-- Exercise 1.4
---------------

instance Show Royalty where
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"


asciiSuit :: Suit -> String
asciiSuit Hearts   = "H"
asciiSuit Diamonds = "D"
asciiSuit Clubs    = "C"
asciiSuit Spades   = "S"

instance Show Suit where
  show = asciiSuit

instance Show Card where
  show (Ace suit) = "A" ++ show suit
  show (Numeric n suit) = show n ++ show suit
  show (Face royal suit) = show royal ++ show suit


-- Bonus Exercise 1.5
---------------------

unicodeSuit :: Suit -> String
unicodeSuit Hearts   = "\9825"
unicodeSuit Diamonds = "\9826"
unicodeSuit Clubs    = "\9827"
unicodeSuit Spades   = "\9824"


-- Exercise 1.6
---------------

royals :: [Royalty]
royals = [Jack ..]

suits :: [Suit]
suits = [minBound .. maxBound]

numbers :: [Int]
numbers = [2 .. 10]


fullDeck :: Deck
fullDeck =
    [ace s | s <- suits]
        ++ [numeric n s | s <- suits, n <- numbers]
        ++ [face r s | s <- suits, r <- royals]



-- Bonus Exercise 1.8
---------------------
multiDeck :: Int -> Deck
multiDeck n
    | n <= 0 = []
    | otherwise = concat (replicate n fullDeck)
