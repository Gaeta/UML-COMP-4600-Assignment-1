module Cards where

-----------------
--- Section 1 ---
-----------------

-- Exercises 1.1 and 1.2
------------------------

-- Suits and Royalty are enumeartions of their possibilities. Cards also have
-- additional information that goes along with each of the three possible cases
-- of a playing card.

-- Here is a definition of the Royalty data type which enumerates between the
-- three royal members (Jack, Queen, King), and derives instances of the basic
-- type classes Eq, Enum, and Bounded.
data Royalty = Jack | Queen | King
  deriving (Eq, Enum, Bounded)

-- Define the Suit and Royalty data types, and derive some instances of basic
-- type classes for them.

-- The Suit data type is a simple enumeration between the four possibilities:
-- Hearts, Diamonds, Clubs, Spades.  Add these constructors to the Suit data
-- type definition, and derive instances of the Eq, Enum, and Bounded type
-- classes.

data Suit -- **FILL IN HERE**

-- To inform the test suite about all 4 of your suits, fill in these top-level
-- functions: 1 for each constructor of the `Suit` enumeration data type.
hearts, diamonds, clubs, spades :: Suit
hearts   = undefined -- **FILL IN HERE**
diamonds = undefined -- **FILL IN HERE**
clubs    = undefined -- **FILL IN HERE**
spades   = undefined -- **FILL IN HERE**

-- The Card data type has three different types of values:
--
--   * An `Ace` Card contains its Suit
--
--   * A `Numeric` Card contains an Int value (specifying its number) and its
--     Suit
--
--   * A `Face` Card contains a Royalty (a choice between Jack, Queen, and King)
--   * and its Suit
--
-- Add constructors following this specification to the Card data type, and
-- derive an instance of Eq type class.

data Card -- **FILL IN HERE**

-- To tell the test suite how to build different kinds of cards, fill in these
-- top-level functions for each of the 3 different `Card` constructors.
ace :: Suit -> Card
ace = undefined -- **FILL IN HERE**

numeric :: Int -> Suit -> Card
numeric = undefined -- **FILL IN HERE**

face :: Royalty -> Suit -> Card
face = undefined -- **FILL IN HERE**


-- Exercise 1.3
---------------

-- A Hand is a type synonym for list of Cards written as [Card].
type Hand = [Card]

-- Define a similar type synonym for a Deck, which is also a list of Cards.

-- **FILL IN HERE**


-- Exercise 1.4
---------------

-- We can give custom behavior to type class methods by defining the instance
-- ourselves.  For example, here is an instance of the Show class for the
-- Royalty type which prints out a one-letter shorthand for each of the members
-- of Royalty.
instance Show Royalty where
  -- show :: Royalty -> String
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"

-- Write an instance of the Show class for the Suit data type by defining a
-- `show` function that returns a different one-letter string for each of the
-- four constructors enumerated in Suit's definition:

asciiSuit :: Suit -> String
asciiSuit suit = undefined

instance Show Suit where
  -- show :: Suit -> String
  show = asciiSuit

-- Write an instance of the Show class for the Card data type by defining a
-- `show` function that pattern-matches on the given card parameter, and prints
-- the appropriate string depending on the constructor that was used to build
-- it.
instance Show Card where
  -- show :: Card -> String
  show card = undefined -- **FILL IN HERE**


-- Bonus Exercise 1.5
---------------------

-- For extra credit, have the show function return the unicode character
-- corresponding to each Suit, rather than an ASCII character.

unicodeSuit :: Suit -> String
unicodeSuit suit = undefined


-- Exercise 1.6
---------------

-- Sometimes, its handy to have a list of the all the possible values of a type,
-- as when starting with all the cards in a full deck.

-- Since the Royalty data type has an instance of the Enum and Bounded type
-- classes, we can use the '..'  enumeration notation to define a list of all
-- members of Royalty.
royals :: [Royalty]
royals = [Jack ..]

-- Define a similar list containing all the values of the Suit data type.
suits :: [Suit]
suits = undefined -- **FILL IN HERE**

-- Define a list of all the numeric values (all numbers between 2 and 10) that
-- might appear on a Numeric card.
numbers :: [Int]
numbers = undefined -- **FILL IN HERE**

-- Now define a list containing ALL the possible playing cards (the order of the
-- list doesn't matter).
fullDeck = undefined -- **FILL IN HERE**

-- Hint: Rather than write down all 52 options by hand, you might find it easier
-- to tell the computer how to generate each combination from the valid
-- ingredients above.  For example, you could use a list comprehension to try
-- all the possible combinations of `suits` and `royals` to generate all 16
-- `Face` Cards.  You could similarly generate lists of all `Numeric` and `Ace`
-- Cards separately, and then append all three lists together.


-- Bonus Exercise 1.8
---------------------

-- Define the function `multiDeck n` which generates a list containing `n`
-- copies of a `fullDeck`
multiDeck n = undefined -- **FILL IN HERE**
