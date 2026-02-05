Assignment 1: Blackjack (Part 1)
================================

Use this project to put together and test your exercises for Assignment 1.
Template files for answering the assignment exercises are all found in the `src`
directory.  Each section from the assignment description corresponds to a
different Haskell source file in `src`:

1. src/Cards.hs: Section 1 on modeling playing cards
2. src/Score.hs: Section 2 on calculating player scores
3. src/Shuffle.hs: Section 3 on shuffling decks

### NOTE

While the provided template code in `src` will compile as-is, the test suite
will have some initial type errors that prevent compilation, because it is
waiting for you to fill in some answers first.

Completing exercises 1.3, and 1.4 will eliminate the compilation errors and let
you run the test suite to check your answers, even if other exercises are
incomplete.  Exercise 1.1 does not need to be fully correct before being able to
run your tests; you only need enough to finish the prerequisite exercise 1.3.

Testing
-------

You can test your answers using the provided test suite found in the `test`
directory.  Passing all tests of mandatory exercises will ensure a %100 score
(or > %100 if some bonus exercises are passed as well).  Any failing test for an
exercise shows that there is some problem in your code.

This test suite can be run automatically using `cabal` with the command

    cabal test
	
or using `stack` with the command

    stack test
	
While the formatting of their output differs somewhat (for example, `stack` uses
colors to differentiate successful versus failing properties), their results
will be the same.

You can pass some additional test options to selectively run only certain tests.
If you want to only run the tests for "regular" exercises, required to finish
the assignment %100, you can use one of the two commands:

    cabal test --test-options="--match Regular"
	
or

    stack test --test-arguments "--match Regular"

Inversely, if you only want to run the tests for "bonus" exercises, which can
earn extra credit, use one of the two commands

    cabal test --test-options="--match Bonus"
	
or

    stack test --test-arguments "--match Bonus"

### CAUTION

For your own good, do not modify anything in the `test` directory!
