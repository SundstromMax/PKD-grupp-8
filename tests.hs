module Tests where

import Test.HUnit ( assertBool, assertEqual, runTestTT, Test(..) )
import Blackjack
import System.Random ( mkStdGen )

testMakeDeck :: Test
testMakeDeck = TestCase $ assertEqual "length of Deck != 52" 52 (length makeDeck)

testRandomizeDeck :: Test
testRandomizeDeck = TestCase $ assertBool "Deck is not randomized" (fst (fisherYates (mkStdGen 20) makeDeck) /= fst (fisherYates (mkStdGen 10) makeDeck))

testDrawCard :: Test
testDrawCard = TestCase $ assertEqual "Did not draw cards" (length $ deck (drawCard $ initState 10)) 50

testCalculateHand :: Test
testCalculateHand = TestCase $ assertEqual "Hand calculated wrong" (calculateHand [Card Ace Spades, Card Two Spades]) 13

testCalculateHand2 = TestCase $ assertEqual "Hand Calculated Wrong" (calculateHand []) 0

testCardValue :: Test
testCardValue = TestCase $ assertEqual "Card value wrong" (cardValue $ Card Ace Spades) 11

testHandToString :: Test
testHandToString = TestCase $ assertEqual "Card value wrong" (handToString [Card Ace Spades, Card Five Hearts]) "Ace Of Spades, Five Of Hearts"

tests :: Test
tests = TestList [TestLabel "test1" testMakeDeck,
                  TestLabel "test2" testRandomizeDeck,
                  TestLabel "test3" testDrawCard,
                  TestLabel "test4" testCalculateHand,
                  TestLabel "test5" testCalculateHand2,
                  TestLabel "test6" testHandToString]

main = do runTestTT tests