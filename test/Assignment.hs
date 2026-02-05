{-# LANGUAGE BlockArguments #-}
module Assignment where

import Grade

assignment :: Grader ()
assignment = do
  section "Section 1: Playing Cards" do
    exercise "Regular Exercise 1.1" do
      part "Suit" $ split 5 do
        must "has a value for hearts"
        must "has a value for diamonds"
        must "has a value for clubs"
        must "has a value for spades"

      part "Card" $ split 5 do
        must "has value for all four ace cards"
        must "has a value for some face cards"
        must "has a value for all face cards"
        must "has a value for some numeric cards"
        must "has a value for all numeric cards"

    bonus $ exercise "Bonus Exercise 1.2" $ check 5 do
      part "numeric" do
        must "allows numeric cards that are in bounds"
        must "rejects numeric cards that are out of bounds"

    exercise "Regular Exercise 1.3" do
      part "Bounded Suit" $ check 1 do
        must "has a minimum bound"
        must "has a maximum bound"

      part "Enum Suit" $ check 1 do
        must "can calculate the next suit"
        must "can calculate the previous suit"

      part "Eq Suit" $ check 1 do
        must "does not equate hearts and diamonds"
        must "does not equate diamonds and clubs"
        must "does not equate clubs and spades"
        must "does not equate clubs and spades"
        must "never equates two different suits"
        must "has only four different suits"

      part "Eq Card" $ check 2 do
        must "never equates aces and faces"
        must "never equates numeric cards and face cards"
        must "never equates numeric cards and aces"
        must "never equates aces with different suits"
        must "never equates faces with different royalty or suits"
        must "never equates numeric cards with different numers or suits"

    exercise "Regular Exercise 1.4" $ check 5 do
      part "Deck" do
        must "is a type alias for a list of cards"

    exercise "Regular Exercise 1.5" do
      part "asciiSuit" $ split 4 do
        must "renders hearts as \"H\""
        must "renders diamonds as \"D\""
        must "renders clubs as \"C\""
        must "renders spades as \"S\""

      part "Show Card" do
        check 1 $ must "renders the 10 of spades as \"10S\" (or with unicode spades)"
        check 1 $ must "renders the Queen of diamonds as \"QD\" (or with unicode diamonds)"
        check 1 $ must "renders aces as \"A\" followed by the suit"
        check 1 $ must "renders faces as the letter of the royal followed by the suit"
        check 2 $ must "renders numeric cards as the number followed by the suit"

    bonus $ exercise "Bonus Exercise 1.6" $ split 5 do
      part "unicodeSuit" do
        must "renders hearts as \"\\9825\""
        must "renders diamonds as \"\\9826\""
        must "renders clubs as \"\\9827\""
        must "renders spades as \"\\9824\""

    exercise "Regular Exercise 1.7" do
      part "suits" $ split 2 do
        must "contains exactly four suits"
        must "contains no duplicate suits"
        must "contains hearts"
        must "contains diamonds"
        must "contains clubs"
        must "contains spades"

      part "numbers" $ split 3 do
        must "contains exactly 9 numbers"
        must "contains no duplicate numbers"
        must "contains all numbers between 2 and 10"

      part "fullDeck" $ split 5 do
        must "contains exactly 52 cards"
        must "contains no duplicate cards"
        must "contains all ace cards"
        must "contains all face cards"
        must "contains all numeric cards"
        must "contains exactly 1 copy of every possible card"

    bonus $ exercise "Bonus Exercise 1.8" $ split 10 do
      part "multiDeck n" do
        must "contains n * 52 cards"
        must "contains every possible card (if n > 0)"
        must "contains exactly n copies of every card"
        must "contains n copies of a full deck"

  section "Section 2: Blackjack Scoring" do
    section "Subsection 2.1: Simple Scoring" do
      exercise "Regular Exercise 2.1" do
        part "cardValue" do
          split 4 do
            must "values the 7 of spades and as 7"
            must "values the 7 of diamonds and as 7"
            must "values the Queen of hearts as 10"
            must "values the Queen of clubs as 10"
            must "values the Jack of spades as 10"
          split 6 do
            must "values all aces as 11"
            must "values all numeric cards as their number"
            must "values all face cards as 10"

      exercise "Regular Exercise 2.2" do
        part "handValue" do
          check 1 $ must "values the empty hand as 0"
          check 2 $ must "values a hand of a single card as the value of that card"
          check 3 $ must "values a hand of two cards as the addition of both cards' values"
          check 4 $ must "values the combination of two hands as the sum of their values"
    bonus $ section "Bonus Section 2.2: Soft Aces" do
      exercise "Bonus Exercise 2.3" do
        part "scoreValue . cardScore" $ check 4 do
          must "is the same as `cardValue` for a single card"

        part "isSoft, isHard" $ split 6 do
          must "are opposites (isSoft s == not (isHard s))"
          must "considers all ace scores as soft"
          must "considers all face scores as hard"
          must "considers all numeric card scores as hard"

      exercise "Bonus Exercise 2.4" $ split 10 do
        part "Monoid Score" do
          must "has a 0-valued empty score"
          must "adds the values of merged (<>) scores"
          must "obeys the left identity law (mempty <> s == s)"
          must "obeys the right identity law (s <> mempty == s)"
          must "obeys the associativity law ((s1 <> s2) <> s3 == s1 <> (s2 <> s3))"
          must "considers the empty score as hard"
          must "consider a score made up of no aces as hard"
          must "considers the combination of two hard scores as hard"
          must "consider a score made up of at least one ace as soft"
          must "considers the combination of a soft score with anythng as hard"

      exercise "Bonus Exercise 2.5" $ split 5 do
        part "handScore" do
          must "considers a hand of [AS, 4H] as a soft 15"
          must "has the same value as `handValue` for any list of cards"
          must "gives the empty score for the empty hand"
          must "is the same as cardScore for a hand of one card"
          must "combines (<>) the scores from any combination of hands (h1 ++ h2)"
    
      exercise "Bonus Exercise 2.6" do
        part "isBust" $ split 1 do
          must "says a score is bust if its value is over 21"
          must "says a score isn't bust if its value is within 21"
        part "Eq Score" $ split 2 do
          must "does not equate scores from differently-valued cards"
          must "does equate scores from same-valued cards"
          must "says any two bust scores are equal (tied)"
          must "(==) equates scores according to `compare`"
        part "Ord Score" $ split 2 do
          must "compares scores from individual cards according to their value"
          must "says any bust score is less than (loses to) any non-bust score"
          must "compares any two non-bust scores according to their value"

      exercise "Bonus Exercise 2.7" do
        part "improveScore" $ split 8 do
          must "leaves a soft hand of [AS, 4H] alone"
          must "changes a soft hand of [7C, AS, 4H] to a hard 12"
          must "changes a soft hand of two aces to a soft 12"
          must "never changes a non-bust score (21 or under)"
          must "never changes a hard score"
          must "lowers the value a soft, bust score (over 21)"
          must "can only make a soft, bust score better, or else hardens it"
          must "is idempotent (never changes a score more than once)"
          must "changes an additional ace to a hard 1 if adding 11 would go over 21"
          must "leaves an additional ace as a soft 11 if it stays 21 or under"
          must "changes an improved hard score + improved soft score by 0 or -10"
          must "changes the sum of any already-improved scores by 0, -10, or -20"
        part "betterHandValue" $ check 2 do
          must "finds the *best* possible score for any hand of cards"

  section "Section 3: Deck Shuffling" do
    exercise "Regular Exercise 3.1" $ split 5 do
      part "at" do
        must "can construct an indexed value"
      part "Show (Indexed i a)" do
        must "can display an indexed value as a string"
      part "item" do
        must "retrieves the `a` item of an `Indexed i a` value"
      part "index" do
        must "retrieves the `i` index of an `Indexed i a` value"

    exercise "Regular Exercise 3.2" $ split 10 do
      part "Eq (Indexed i a)" do
        must "only equates values based on the index i (and ignores the a)"
      part "Ord (Indexed i a)" do
        must "only compared values based on the index i (and ignores the a)"
            
    exercise "Regular Exercise 3.3" $ split 10 do
      part "addIndexes" do
        must "addIndexes [3, 2, 1] \"abc\" == ['a' `at` 3, 'b' `at` 2, 'c' `at` 1]"
        must "returns a list with the same length as the shorter argument"
        must "truncates the longer of the two arguments"
        must "keeps every index from the first list (given enough items)"
        must "keeps every item from the second list (given enough indexes)"
      part "removeIndexes" do
        must "removeIndexes ['a' `at` 3, 'b' `at` 2, 'c' `at` 1] == \"abc\""
        must "returns a list with the same length as its argument"
        must "keeps every item from the argument list"
      part "removeIndexes . addIndexes ixs" do
        must "is the identity function (if `ixs` is as long as the item list)"
        must "truncates the item list to the same length as `ixs`"

    exercise "Regular Exercise 3.4" do
      part "shuffle" do
        check 1 $ must "shuffle [4, 2, 7, 3, 6, 9] \"abcdef\" == \"bdaecf\""
        check 2 $ must "returns a list whose length is the min of its two arguments"
        check 2 $ must "truncates the longer of the two arguments"
        check 4 $ must "permutes a list (given as many indexes as items)"
        check 3 $ must "returns the same list, with enough indexes in ascending order"
        check 3 $ must "reverses the list, with enough indexes in decending order"
