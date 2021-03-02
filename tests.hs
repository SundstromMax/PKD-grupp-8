module Tests where

import Test.HUnit ( assertBool, assertEqual, runTestTT, Test(..) )
import Blackjack
import System.Random ( mkStdGen )

{-Tests if makedeck creates the right amount of cards-}
testMakeDeck :: Test
testMakeDeck = TestCase $ assertEqual "length of Deck != 52" 52 (length makeDeck)

{-Tests if the deck is randomized-}
testRandomizeDeck :: Test
testRandomizeDeck = TestCase $ assertBool "Deck is not randomized" (fst (fisherYates (mkStdGen 20) makeDeck) /= fst (fisherYates (mkStdGen 10) makeDeck))

{-Tests if drawCard removes two cards from deck-}
testDrawCard :: Test
testDrawCard = TestCase $ assertEqual "Did not draw cards" (length $ deck (drawCard $ initState 10)) 50

{-Tests if the value of the hand is added correctly-}
testCalculateHand :: Test
testCalculateHand = TestCase $ assertEqual "Hand calculated wrong" (calculateHand [Card Ace Spades, Card Two Spades]) 3
testCalculateHand2 = TestCase $ assertEqual "Hand Calculated Wrong" (calculateHand []) 0

{-Tests if the cards have the right values-}
testCardValue :: Test
testCardValue = TestCase $ assertEqual "Card value wrong" (cardValue $ Card Ace Spades) 1
testCardValue2 = TestCase $ assertEqual "Card value wrong" (cardValue $ Card King Spades) 10

{-Tests if the toString function works correctly-}
testHandToString :: Test
testHandToString = TestCase $ assertEqual "Card value wrong" (handToString [Card Ace Spades, Card Five Hearts]) "Ace Of Spades\n     | Five Of Hearts"

{-Tests if the hasAce works correctly-}
testHasAce :: Test
testHasAce = TestCase $ assertBool "Found an ace when it shouldn't" (hasAce [] == False)
testHasAce2 = TestCase $ assertBool "Couldn't find ace" (hasAce [Card Three Spades, Card Ace Hearts])

{-Tests if the hand containing an ace is added correctly -}
testCalculateAceHand :: Test
testCalculateAceHand = TestCase $ assertEqual "hand calculated wrong" (calculateAceHand [Card Ace Spades, Card Ace Hearts]) 12
testCalculateAceHand2 = TestCase $ assertEqual "hand calculated wrong" (calculateAceHand []) 0


tests :: Test
tests = TestList [TestLabel "test1" testMakeDeck,
                  TestLabel "test2" testRandomizeDeck,
                  TestLabel "test3" testDrawCard,
                  TestLabel "test4" testCalculateHand,
                  TestLabel "test5" testCalculateHand2,
                  TestLabel "test6" testHandToString,
                  TestLabel "test7" testHasAce,
                  TestLabel "test8" testHasAce2,
                  TestLabel "test9" testCalculateAceHand,
                  TestLabel "test10" testCalculateAceHand2,
                  TestLabel "test11" testCardValue,
                  TestLabel "test12" testCardValue2]

main = do runTestTT tests