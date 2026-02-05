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

module Score where

import Cards

-------------------
--- Section 2.1 ---
-------------------

-- Exercise 2.1
---------------

cardValue :: Card -> Int
cardValue (Ace _) = 11
cardValue (Numeric n _) = n
cardValue (Face _ _) = 10

-- Exercise 2.2
---------------

handValue :: Hand -> Int
handValue hand = sum (map cardValue hand)


-------------------------
--- Bonus Section 2.2 ---
-------------------------

-- Bonus Exercise 2.3
---------------------

data Score = Score
    {
        hardPoints :: Int,
        softAces :: Int
    }
    deriving (Show)

cardScore :: Card -> Score
cardScore (Ace _) = Score 1 1
cardScore (Numeric n _) = Score n 0
cardScore (Face _ _) = Score 10 0

scoreValue :: Score -> Int
scoreValue (Score h s) = h + (s * 10)

isSoft, isHard :: Score -> Bool
isSoft (Score _ s) = s > 0
isHard = not . isSoft


-- Bonus Exercise 2.4
---------------------

instance Eq Score where
    score1 == score2 = compare score1 score2 == EQ

instance Ord Score where
    compare score1 score2
        | isBust score1 && isBust score2 = EQ
        | isBust score1 && not (isBust score2) = LT
        | not (isBust score1) && isBust score2 = GT
        | otherwise = compare (scoreValue score1) (scoreValue score2)

instance Semigroup Score where
    (Score h1 s1) <> (Score h2 s2) = Score (h1 + h2) (s1 + s2)

instance Monoid Score where
    mempty = Score 0 0


-- Bonus Exercise 2.5
---------------------

handScore :: Hand -> Score
handScore hand = mconcat (map cardScore hand)


-- Bonus Exercise 2.6
---------------------

isBust :: Score -> Bool
isBust score = scoreValue score >= 22


-- Bonus Exercise 2.7
---------------------

improveScore :: Score -> Score
improveScore score@(Score _ s)
    | scoreValue score <= 21 = score
    | s <= 0 = score
    | otherwise =
        let reduceOnce (Score h' s') = Score h' (s' - 1)
            step sc =
                if isBust sc && isSoft sc
                    then step (reduceOnce sc)
                    else sc
        in step score

betterHandValue :: Hand -> Int
betterHandValue hand = scoreValue (improveScore (handScore hand))
