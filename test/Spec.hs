{-# LANGUAGE BlockArguments #-}
module Spec where

import Cards
import Score
import Shuffle
import Data.Maybe (isNothing)
import Data.List (sort)
import Data.Function (on)
import Test.QuickCheck hiding (shuffle)
import Test.QuickCheck.Exception
import Test.QuickCheck.Poly
import Test.Hspec
import Test.Hspec.Expectations

test :: Spec
test = do
  context "Section 1: Playing Cards" do
    context "Regular Exercise 1.1" do
      describe "Suit" do
        it "has a value for hearts" do
          evaluate hearts
          return ()
        it "has a value for diamonds" do
          evaluate diamonds
          return ()
        it "has a value for clubs" do
          evaluate clubs
          return ()
        it "has a value for spades" do
          evaluate spades
          return ()

      describe "Card" do
        it "has value for all four ace cards" do
          evaluate (ace hearts)
          evaluate (ace diamonds)
          evaluate (ace clubs)
          evaluate (ace spades)
          return ()
        it "has a value for some face cards" do
          evaluate (face Jack clubs)
          evaluate (face Queen hearts)
          evaluate (face King diamonds)
          return ()
        it "has a value for all face cards" do
          sequence_
            [ evaluate (face f s)
            | f <- everything,
              s <- everything ]
        it "has a value for some numeric cards" do
          evaluate (numeric 2 clubs)
          evaluate (numeric 10 spades)
          evaluate (numeric 8 diamonds)
          return ()
        it "has a value for all numeric cards" do
          sequence_
            [ evaluate (numeric n s)
            | n <- [2..10],
              s <- everything ]

    context "Bonus Exercise 1.2" do
      describe "numeric" do
        it "allows numeric cards that are in bounds" $
          sequence_
            [ evaluate (numeric n s)
            | n <- [2..10],
              s <- everything ]
        it "rejects numeric cards that are out of bounds" $
          property
          \n s -> (n < 2 || 10 < n)
                  ==>
                  evaluate (numeric n s)
                  `shouldThrow`
                  anyException

    context "Regular Exercise 1.3" do
      describe "Bounded Suit" do
        it "has a minimum bound" do
          evaluate minSuit
          return ()
        it "has a maximum bound" do
          evaluate maxSuit
          return ()

      describe "Enum Suit" do
        it "can calculate the next suit" $
          property compareNextSuit
        it "can calculate the previous suit" $
          property comparePrevSuit

      describe "Eq Suit" do
        eqEquivalenceProp (undefined :: Suit)
        it "does not equate hearts and diamonds" $
          property $ hearts =/= diamonds
        it "does not equate diamonds and clubs" $
          property $ diamonds =/= clubs
        it "does not equate clubs and spades" $
          property $ clubs =/= spades
        it "does not equate clubs and spades" $
          property $ spades =/= hearts
        it "never equates two different suits" $
          property (\s -> s =/= nextSuit s)
          .&.
          property (\s -> s =/= nextSuit (nextSuit s))
          .&.
          property (\s -> s =/= nextSuit (nextSuit (nextSuit s)))
        it "has only four different suits" $
          property \s -> s === nextSuit (nextSuit (nextSuit (nextSuit s)))

      describe "Eq Card" do
        eqEquivalenceProp (undefined :: Card)
        it "never equates aces and faces" $
          property
          \s1 r2 s2 -> ace s1 =/= face r2 s2
        it "never equates numeric cards and face cards" $
          property
          \(CardNum n1) s1 r2 s2 -> numeric n1 s1 =/= face r2 s2
        it "never equates numeric cards and aces" $
          property
          \(CardNum n1) s1 s2 -> numeric n1 s1 =/= ace s2
        it "never equates aces with different suits" $
          property
          \s1 s2 -> (s1 /= s2) ==> ace s1 =/= ace s2
        it "never equates faces with different royalty or suits" $
          property
          \r1 s1 r2 s2 -> (r1 /= r2 || s1 /= s2)
                          ==>
                          face r1 s1 =/= face r2 s2
        it "never equates numeric cards with different numers or suits" $
          property
          \(CardNum n1) s1 (CardNum n2) s2 ->
            (n1 /= n2 || s1 /= s2)
            ==>
            numeric n1 s1 =/= numeric n2 s2

    context "Regular Exercise 1.4" do
      describe "Deck" do
        it "is a type alias for a list of cards" do
          let d = [numeric 7 spades] :: Deck
          evaluate d
          return ()

    context "Regular Exercise 1.5" do
      describe "asciiSuit" do
        it "renders hearts as \"H\"" $
          property $ asciiSuit hearts === "H"
        it "renders diamonds as \"D\"" $
          property $ asciiSuit diamonds === "D"
        it "renders clubs as \"C\"" $
          property $ asciiSuit clubs === "C"
        it "renders spades as \"S\"" $
          property $ asciiSuit spades === "S"

      describe "Show Card" do
        it ("renders the 10 of spades as \"10S\" (or with unicode spades)") $
          property $ show (numeric 10 spades) === "10S"
                .||. show (numeric 10 spades) === "10\9824"
        it ("renders the Queen of diamonds as \"QD\" (or with unicode diamonds)") $
          property $ show (face Queen diamonds) === "QD"
                .||. show (face Queen diamonds) === "Q\9826"
        it "renders aces as \"A\" followed by the suit" $
          property
          \s -> show (ace s) === "A" ++ show s
        it "renders faces as the letter of the royal followed by the suit" $
          property
          \r s -> show (face r s) === show r ++ show s
        it "renders numeric cards as the number followed by the suit" $
          property
          \(CardNum n) s -> show (numeric n s) === show n ++ show s

    context "Bonus Exercise 1.6" do
      describe "unicodeSuit" do
        it "renders hearts as \"\\9825\"" $
          property $ unicodeSuit hearts === "\9825"
        it "renders diamonds as \"\\9826\"" $
          property $ unicodeSuit diamonds === "\9826"
        it "renders clubs as \"\\9827\"" $
          property $ unicodeSuit clubs === "\9827"
        it "renders spades as \"\\9824\"" $
          property $ unicodeSuit spades === "\9824"

    context "Regular Exercise 1.7" do
      describe "suits" do
        it "contains exactly four suits" $
          property $ length suits === 4
        it "contains no duplicate suits" $
          property $ isNothing (findDup suits)
        it "contains hearts" $
          property $ hearts `elem` suits
        it "contains diamonds" $
          property $ diamonds `elem` suits
        it "contains clubs" $
          property $ clubs `elem` suits
        it "contains spades" $
          property $ spades `elem` suits

      describe "numbers" do
        it "contains exactly 9 numbers" $
          property $ length numbers === 9
        it "contains no duplicate numbers" $
          property $ isNothing (findDup suits)
        it "contains all numbers between 2 and 10" $
          property $ \(CardNum n) -> n `elem` numbers

      describe "fullDeck" do
        it "contains exactly 52 cards" $
          property $ length fullDeck' === 52
        it "contains no duplicate cards" $
          property $ isNothing (findDup fullDeck')
        it "contains all ace cards" $
          property $ \s -> ace s `elem` fullDeck'
        it "contains all face cards" $
          property $ \r s -> face r s `elem` fullDeck'
        it "contains all numeric cards" $
          property $
          \(CardNum n) s -> numeric n s `elem` fullDeck'
        it "contains exactly 1 copy of every possible card" $
          property $ \c -> copies 1 c fullDeck'

    context "Bonus Exercise 1.8" do
      describe "multiDeck n" do
        it "contains n * 52 cards" $
          property
          \n -> (n >= 0) ==> (length (multiDeck' n) === n * 52)
        it "contains every possible card (if n > 0)" $
          property
          \n c -> (n > 0) ==> (c `elem` multiDeck' n)
        it "contains exactly n copies of every card" $
          property
          \n c -> (n >= 0) ==> copies n c (multiDeck' n)
        it "contains n copies of a full deck" $
          property
          \n -> (n >= 0)
                ==>
                let d = multiDeck' n
                in conjoin [ copies n c d | c <- fullDeck' ]

  context "Section 2: Blackjack Scoring" do
    context "Subsection 2.1: Simple Scoring" do
      context "Regular Exercise 2.1" do
        describe "cardValue" do
          it "values the 7 of spades and as 7" $
            property $ cardValue (numeric 7 spades) === 7
          it "values the 7 of diamonds and as 7" $
            property $ cardValue (numeric 7 diamonds) === 7
          it "values the Queen of hearts as 10" $
            property $ cardValue (face Queen hearts) === 10
          it "values the Queen of clubs as 10" $
            property $ cardValue (face Queen clubs) === 10
          it "values the Jack of spades as 10" $
            property $ cardValue (face Jack spades) === 10
          it "values all aces as 11" $
            property $ \s -> cardValue (ace s) === 11
          it "values all numeric cards as their number" $
            property $ \(CardNum n) s -> cardValue (numeric n s) === n
          it "values all face cards as 10" $
            property $ \r s -> cardValue (face r s) === 10

      context "Regular Exercise 2.2" do
        describe "handValue" do
          it "values the empty hand as 0" $
            property $ handValue [] === 0
          it "values a hand of a single card as the value of that card" $
            property
            \c -> handValue [c] === cardValue c
          it "values a hand of two cards as the addition of both cards' values" $
            property
            \c1 c2 -> handValue [c1, c2] === cardValue c1 + cardValue c2
          it "values the combination of two hands as the sum of their values" $
            property
            \h1 h2 -> handValue (h1 ++ h2) === handValue h1 + handValue h2

    context "Bonus Section 2.2: Soft Aces" do
      context "Bonus Exercise 2.3" do
        describe "scoreValue . cardScore" do
          it "is the same as `cardValue` for a single card" $
            property \c -> scoreValue (cardScore c) === cardValue c

        describe "isSoft, isHard" do
          it "are opposites (isSoft s == not (isHard s))" $
            property \s -> isSoft s === not (isHard s)
          it "considers all ace scores as soft" do
            property \s -> isSoft (cardScore (ace s))
          it "considers all face scores as hard" do
            property \r s -> isHard (cardScore (face r s))
          it "considers all numeric card scores as hard" do
            property \(CardNum n) s -> isHard (cardScore (numeric n s))

      context "Bonus Exercise 2.4" do
        describe "Monoid Score" do
          it "has a 0-valued empty score" $
            property $ scoreValue mempty === 0
          it "adds the values of merged (<>) scores" $
            property
            \s1 s2 -> scoreValue (s1 <> s2)
                      ===
                      (scoreValue s1) + (scoreValue s2)
          it "obeys the left identity law (mempty <> s == s)" $
            property
            \s -> mempty <> s === (s :: Score)
          it "obeys the right identity law (s <> mempty == s)" $
            property
            \s -> s <> mempty === (s :: Score)
          it "obeys the associativity law ((s1 <> s2) <> s3 == s1 <> (s2 <> s3))" $
            property
            \s1 s2 s3 -> (s1 <> s2) <> s3 === s1 <> (s2 <> s3 :: Score)
          it "considers the empty score as hard" $
            property $ isHard (mempty :: Score)
          it "consider a score made up of no aces as hard" $
            property \(HardScore s) -> isHard s
          it "considers the combination of two hard scores as hard" $
            property
            \(HardScore s1) (HardScore s2) -> isHard (s1 <> s2)
          it "consider a score made up of at least one ace as soft" $
            property \(SoftScore s) -> isSoft s
          it "considers the combination of a soft score with anythng as hard" $
            property (\(SoftScore s1) s2 -> isSoft (s1 <> s2))
            .&&.
            property (\s1 (SoftScore s2) -> isSoft (s1 <> s2))

      context "Bonus Exercise 2.5" do
        describe "handScore" do
          it "considers a hand of [AS, 4H] as a soft 15" $
            property $ let s = handScore [ace spades, numeric 4 diamonds]
                       in isSoft s .&&. scoreValue s === 15
          it "has the same value as `handValue` for any list of cards" $
            property $ \h -> scoreValue (handScore h) === handValue h
          it "gives the empty score for the empty hand" do
            property $ handScore [] === mempty
          it "is the same as cardScore for a hand of one card" do
            property $ \c -> handScore [c] === cardScore c
          it "combines (<>) the scores from any combination of hands (h1 ++ h2)" $
            property $
            \h1 h2 -> handScore (h1 ++ h2) === handScore h1 <> handScore h2
    
      context "Bonus Exercise 2.6" do
        describe "isBust" do
          it "says a score is bust if its value is over 21" $
            property $ \(BustScore s) -> isBust s
          it "says a score isn't bust if its value is within 21" $
            property $ \s -> (scoreValue s <= 21) ==> not (isBust s)
        describe "Eq Score" do
          eqEquivalenceProp (undefined :: Score)
          it "does not equate scores from differently-valued cards" $
            property
            \c1 c2 -> cardValue c1 /= cardValue c2
                      ==>
                      cardScore c1 =/= cardScore c2
          it "does equate scores from same-valued cards" $
            property
            \c1 c2 -> cardValue c1 == cardValue c2
                      ==>
                      cardScore c1 === cardScore c2
          it "says any two bust scores are equal (tied)" $
            property
            \(BustScore s1) (BustScore s2) -> s1 === s2
          it "(==) equates scores according to `compare`" $
            property
            \s1 s2 -> (compare s1 s2 == EQ) === (s1 == (s2 :: Score))
        describe "Ord Score" do
          it "compares scores from individual cards according to their value" $
            property
            \c1 c2 -> compare (cardValue c1) (cardValue c2)
                      ===
                      compare (cardScore c1) (cardScore c2)
          it "says any bust score is less than (loses to) any non-bust score" $
            property
            \(BustScore s1) s2 -> not (isBust s2)
                                  ==>
                                  s1 < s2
          it "compares any two non-bust scores according to their value" $
            property
            \(NonBlackjackScore s1) (NonBlackjackScore s2) ->
              not (isBust s1) && not (isBust s2)
              ==>
              compare s1 s2
              ===
              compare (scoreValue s1) (scoreValue s2)

      context "Bonus Exercise 2.7" do
        describe "improveScore" do
          it "leaves a soft hand of [AS, 4H] alone" $
            property $
            let s = handScore [ace spades, numeric 4 diamonds]
                s' = improveScore s
            in isSoft s' .&&. scoreValue s' === 15
          it "changes a soft hand of [7C, AS, 4H] to a hard 12" $
            property $
            let s = handScore [numeric 7 clubs, ace spades, numeric 4 diamonds]
                s' = improveScore s
            in isHard s' .&&. scoreValue s' === 12
          it "changes a soft hand of two aces to a soft 12" $
            property
            \a1 a2 ->
              let s = improveScore (handScore [ace a1, ace a2])
              in scoreValue s === 12 .&&. isSoft s
          it "never changes a non-bust score (21 or under)" $
            property \s -> scoreValue s <= 21
                           ==>
                           improveScore s === s
          it "never changes a hard score" $
            property \(HardScore s) -> improveScore s === s
          it "lowers the value a soft, bust score (over 21)" $
            property
            \(SoftScore s) -> isBust s
                              ==>
                              scoreValue (improveScore s) < scoreValue s
          it "can only make a soft, bust score better, or else hardens it" $
            property
            \(SoftScore s) -> isBust s
                              ==>
                              let s' = improveScore s
                              in compare s' s === GT
                                 .||.
                                 (isHard s' .&&. s' === s)
          it "is idempotent (never changes a score more than once)" $
            property $ \s -> improveScore (improveScore s) === improveScore s
          it "changes an additional ace to a hard 1 if adding 11 would go over 21" $
            property
            \a (HardScore s) ->
              let s1 = improveScore (cardScore (ace a) <> s)
                  s2 = improveScore (s <> cardScore (ace a))
              in (scoreValue s + 11 > 21)
                 ==>
                 isHard s1 .&&. (scoreValue s1 === scoreValue s + 1)
                 .&&.
                 isHard s2 .&&. (scoreValue s2 === scoreValue s + 1)
          it "leaves an additional ace as a soft 11 if it stays 21 or under" $
            property
            \a s ->
              let s1 = improveScore (cardScore (ace a) <> s)
                  s2 = improveScore (s <> cardScore (ace a))
              in (scoreValue s + 11 <= 21)
                 ==>
                 isSoft s1 .&&. (scoreValue s1 === scoreValue s + 11)
                 .&&.
                 isSoft s2 .&&. (scoreValue s2 === scoreValue s + 11)
          it "changes an improved hard score + improved soft score by 0 or -10" $
            property
            \(HardScore s1) (SoftScore s2) ->
              let s1' = improveScore s1
                  s2' = improveScore s2
                  t1' = improveScore (s1' <> s2')
                  t2' = improveScore (s2' <> s1')
              in (scoreValue t1' === scoreValue s1' + scoreValue s2'
                  .&&.
                  scoreValue t2' === scoreValue s2' + scoreValue s1')
                 .||.
                 (scoreValue t1' === scoreValue s1' + scoreValue s2' - 10
                  .&&.
                  scoreValue t2' === scoreValue s2' + scoreValue s1' - 10)
          it "changes the sum of any already-improved scores by 0, -10, or -20" $
            property
            \s1 s2 ->
              let s1' = improveScore s1
                  s2' = improveScore s2
                  s'  = improveScore (s1' <> s2')
              in scoreValue s' === scoreValue s1' + scoreValue s2'
                 .||.
                 scoreValue s' === scoreValue s1' + scoreValue s2' - 10
                 .||.
                 scoreValue s' === scoreValue s1' + scoreValue s2' - 20
        describe "betterHandValue" do
          it "finds the *best* possible score for any hand of cards" $
            property $
            \h -> betterHandValue (take 12 h) === bestHandValue (take 12 h)

  context "Section 3: Deck Shuffling" do
    context "Regular Exercise 3.1" do
      describe "at" do
        it "can construct an indexed value" $
          property
          \a i -> do
            evaluate ((a :: A) `at` (i :: B))
            return ()
      describe "Show (Indexed i a)" do
        it "can display an indexed value as a string" $
          property
          \a i -> do
            evaluate (show $ (a :: A) `at` (i :: B))
            return ()
      describe "item" do
        it "retrieves the `a` item of an `Indexed i a` value" $
          property
          \a i -> item (a `at` (i :: B)) === (a :: A)
      describe "index" do
        it "retrieves the `i` index of an `Indexed i a` value" $
          property
          \a i -> index ((a :: A) `at` i) === (i :: B)

    context "Regular Exercise 3.2" do
      describe "Eq (Indexed i a)" do
        it "only equates values based on the index i (and ignores the a)" $
          property
          \x y ->
            (x == (y :: Indexed B A))
            ===
            ((==) `on` index) x y
      describe "Ord (Indexed i a)" do
        it "only compared values based on the index i (and ignores the a)" $
          property
          \x y ->
            compare x (y :: Indexed OrdB A)
            ===
            (compare `on` index) x y
            
    context "Regular Exercise 3.3" do
      describe "addIndexes" do
        it "addIndexes [3, 2, 1] \"abc\" == ['a' `at` 3, 'b' `at` 2, 'c' `at` 1]" $
          property $
          addIndexes [3, 2, 1] "abc"
          ===
          ['a' `at` 3, 'b' `at` 2, 'c' `at` 1]
        it "returns a list with the same length as the shorter argument" $
          property
          \is as ->
            length (addIndexes (is :: [B]) (as :: [A]))
            ===
            min (length is) (length as)
        it "truncates the longer of the two arguments" $
          property
          \is as ->
            addIndexes (is :: [B]) (as :: [A])
            ===
            addIndexes (take (length as) is) (take (length is) as)
        it "keeps every index from the first list (given enough items)" $
          property
          \is as ->
            let xs = addIndexes is as :: [Indexed B A]
            in length is <= length as
               ==>
               conjoin
               [ disjoin [ i === index x | x <- xs ]
               | i <- is ]
        it "keeps every item from the second list (given enough indexes)" $
          property
          \is as ->
            let xs = addIndexes is as :: [Indexed B A]
            in length as <= length is
               ==>
               conjoin
               [ disjoin [ a === item x | x <- xs ]
               | a <- as ]
      describe "removeIndexes" do
        it "removeIndexes ['a' `at` 3, 'b' `at` 2, 'c' `at` 1] == \"abc\"" $
          property $ removeIndexes ['a' `at` 3, 'b' `at` 2, 'c' `at` 1] == "abc"
        it "returns a list with the same length as its argument" $
          property
          \xs -> length (removeIndexes xs) === length (xs :: [Indexed B A])
        it "keeps every item from the argument list" $
          property
          \xs -> let as = removeIndexes (xs :: [Indexed B A])
                 in conjoin
                    [ item x `elem` as | x <- xs]
      describe "removeIndexes . addIndexes ixs" do
        it "is the identity function (if `ixs` is as long as the item list)" $
          property
          \ixs as -> length (ixs :: [B]) >= length (as :: [A])
                    ==>
                    removeIndexes (addIndexes ixs as) === as
        it "truncates the item list to the same length as `ixs`" $
          property
          \ixs as -> removeIndexes (addIndexes ixs as :: [Indexed B A])
                     ===
                     take (length ixs) as

    context "Regular Exercise 3.4" do
      describe "shuffle" do
        it "shuffle [4, 2, 7, 3, 6, 9] \"abcdef\" == \"bdaecf\"" $
          property $ shuffle [4, 2, 7, 3, 6, 9] "abcdef" == "bdaecf"
        it "returns a list whose length is the min of its two arguments" $
          property
          \is as -> length (shuffle is (as :: [A]))
                    ===
                    min (length as) (length is)
        it "truncates the longer of the two arguments" $
          property
          \is as -> shuffle is (as :: [A])
                    ===
                    shuffle (take (length as) is) (take (length is) as)
        it "permutes a list (given as many indexes as items)" $
          property
          \is as -> length is >= length (as :: [OrdA])
                    ==>
                    sort as === sort (shuffle is as)
        it "returns the same list, with enough indexes in ascending order" $
          property
          \n k as -> shuffle [n,n+(1 + abs k)..] as === (as :: [A])
        it "reverses the list, with enough indexes in decending order" $
          property
          \n k as-> shuffle [n,n-(1 + abs k)..] as === reverse (as :: [A])

fullDeck' :: [Card]
fullDeck' = fullDeck

multiDeck' :: Int -> [Card]
multiDeck' = multiDeck

maxSuit :: Suit
maxSuit = maxBound

minSuit :: Suit
minSuit = minBound

nextSuit s | fromEnum s == fromEnum maxSuit = minSuit
           | otherwise                      = succ s

prevSuit s | fromEnum s == fromEnum minSuit = maxSuit
           | otherwise                      = pred s

compareNextSuit s = (s /= maxSuit)
                    ==>
                    (fromEnum s + 1 === fromEnum (succ s))

comparePrevSuit s = (s /= minSuit)
                    ==>
                    (fromEnum s - 1 === fromEnum (pred s))

everything :: (Enum a, Bounded a) => [a]
everything = [minBound .. maxBound]

hasType :: a -> a -> a
x `hasType` _ = x

eqEquivalenceProp proxy = do
  it "is reflexive (everything is equal to itself)" $
    property (\x -> eqReflexive (x `hasType` proxy))
  it "is symmetric (equality is reversible)" $
    property (\x -> eqSymmetric (x `hasType` proxy))
  it "is transitive (equality chains together)" $
    property (\x -> eqTransitive (x `hasType` proxy))

eqReflexive x = x === x

eqSymmetric x y = (x == y) === (y == x)

eqTransitive x y z = if x == y && y == z
                     then x === z
                     else property True

findDup []      = Nothing
findDup (x:xs)
  | x `elem` xs = Just x
  | otherwise   = findDup xs

copies 0 y xs = all (/= y) xs
copies n y [] = False
copies n y (x:xs)
  | x == y    = copies (n-1) y xs
  | otherwise = copies n y xs

allHandValues [] = [0]
allHandValues (c:cs)
  | c `elem` aces = do n <- allHandValues cs
                       [n + 1, n + 11]
  | otherwise     = [ n + cardValue c | n <- allHandValues cs ]
  where aces = [ ace hearts, ace diamonds, ace clubs, ace spades ]

bestHandValue h
  | null ok   = minimum bust
  | otherwise = maximum ok
  where options = allHandValues h
        ok = filter (<= 21) options
        bust = filter (> 21) options

blackjackHands a r s =
  [ [ace a, c] | c <- [face r s, numeric 10 s] ]
  ++
  [ [c, ace a] | c <- [face r s, numeric 10 s] ]


newtype CardNumber a = CardNum a
  deriving (Eq, Ord, Show)

getNum :: CardNumber a -> a
getNum (CardNum n) = n

instance Num a => Arbitrary (CardNumber a) where
  arbitrary = CardNum <$> fromInteger <$> chooseInteger (2, 10)

instance Arbitrary Royalty where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Suit where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Card where
  arbitrary = oneof
              [ ace <$> arbitrary,
                face <$> arbitrary <*> arbitrary,
                numeric <$> (getNum <$> arbitrary) <*> arbitrary ]

instance Arbitrary Score where
  arbitrary = oneof
              [ pure mempty,
                cardScore <$> arbitrary,
                (<>) <$> arbitrary <*> arbitrary ]

newtype HardScore = HardScore Score
  deriving (Eq, Show)

onlyHard :: HardScore -> Score
onlyHard (HardScore s) = s

instance Arbitrary HardScore where
  arbitrary =
    HardScore <$>
    oneof
    [ pure mempty,
      cardScore <$> (face <$> arbitrary <*> arbitrary),
      cardScore <$> (numeric <$> (getNum <$> arbitrary) <*> arbitrary),
      (<>) <$> (onlyHard <$> arbitrary) <*> (onlyHard <$> arbitrary) ]

newtype SoftScore = SoftScore Score
  deriving (Eq, Show)

onlySoft :: SoftScore -> Score
onlySoft (SoftScore s) = s          

newtype BustScore = BustScore Score
  deriving (Eq, Show)

onlyBust :: BustScore -> Score
onlyBust (BustScore s) = s

instance Arbitrary BustScore where
  arbitrary = do s <- arbitrary
                 keepGoing s
    where keepGoing :: Score -> Gen BustScore
          keepGoing s
            | isBust s  = return (BustScore s)
            | otherwise = do
                c <- arbitrary
                keepGoing (cardScore c <> s)

newtype BlackjackScore = BlackjackScore Score
  deriving (Eq, Show)

onlyBlackjack :: BlackjackScore -> Score
onlyBlackjack (BlackjackScore s) = s

instance Arbitrary BlackjackScore where
  arbitrary =
    BlackjackScore <$>
    oneof
    (concat
      [ [(<>) <$> ten <*> eleven, (<>) <$> eleven <*> ten]
      | ten <- tens ])
    where tens = [ cardScore <$> c
                 | c <- [numeric 10 <$> arbitrary, face <$> arbitrary <*> arbitrary] ]
          eleven = cardScore <$> (ace <$> arbitrary)

newtype NonBlackjackScore = NonBlackjackScore Score
  deriving (Eq, Show)

neverBlackjack :: NonBlackjackScore -> Score
neverBlackjack (NonBlackjackScore s) = s

instance Arbitrary NonBlackjackScore where
  arbitrary =
    NonBlackjackScore <$>
    oneof
    [ pure mempty,
      cardScore <$> arbitrary,
      stableNonBlackjack ]
    where nonBlackjackPair =
            [ (ace <$> arbitrary, ace <$> arbitrary),
              (numeric <$> (getNum <$> arbitrary) <*> arbitrary,
               numeric <$> (getNum <$> arbitrary) <*> arbitrary),
              (numeric <$> (getNum <$> arbitrary) <*> arbitrary,
               face <$> arbitrary <*> arbitrary),
              (face <$> arbitrary <*> arbitrary,
               numeric <$> (getNum <$> arbitrary) <*> arbitrary),
              (face <$> arbitrary <*> arbitrary, face <$> arbitrary <*> arbitrary) ]
          stableNonBlackjack =
            oneof
            [ oneof
              [ (<>) <$> (cardScore <$> c1) <*> (cardScore <$> c2)
              | (c1, c2) <- nonBlackjackPair ],
              (<>) <$> arbitrary <*> stableNonBlackjack,
              (<>) <$> stableNonBlackjack <*> arbitrary ]

instance Arbitrary SoftScore where
  arbitrary = SoftScore <$>
              oneof
              [ cardScore <$> ace <$> arbitrary,
                (<>) <$> (onlySoft <$> arbitrary) <*> arbitrary,
                (<>) <$> arbitrary <*> (onlySoft <$> arbitrary) ]

instance (Arbitrary i, Arbitrary a) => Arbitrary (Indexed i a) where
  arbitrary = at <$> arbitrary <*> arbitrary
