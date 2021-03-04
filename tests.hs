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
testDrawCard2 = TestCase $ assertEqual "Did not draw cards" (length $ deck ( drawCard $ drawCard $ initState 20)) 48

{-Tests if the value of the hand is added correctly-}
testCalculateHand :: Test
testCalculateHand = TestCase $ assertEqual "Hand calculated wrong" (calculateHand [Card Ace Spades, Card Two Spades]) 3
testCalculateHand2 = TestCase $ assertEqual "Hand Calculated Wrong" (calculateHand []) 0
testCalculateHand3 = TestCase $ assertEqual "Hand calculated wrong" (calculateHand [Card Ace Spades, Card Two Spades, Card King Hearts, Card Queen Spades]) 23
testCalculateHand4 = TestCase $ assertEqual "Hand calculated wrong" (calculateHand [Card Ace Spades, Card Two Spades, Card Three Hearts, Card Four Clubs]) 10
testCalculateHand5 = TestCase $ assertEqual "Hand calculated wrong" (calculateHand [Card Jack Spades, Card Ten Diamonds, Card Seven Hearts, Card Four Clubs]) 31

{-Tests if the cards have the right values. This function is tested also in the calculate hand function above. -}
testCardValue :: Test
testCardValue = TestCase $ assertEqual "Card value wrong" (cardValue $ Card Ace Spades) 1
testCardValue2 = TestCase $ assertEqual "Card value wrong" (cardValue $ Card King Spades) 10
testCardValue3 = TestCase $ assertEqual "Card value wrong" (cardValue $ Card Jack Clubs) 10
testCardValue4 = TestCase $ assertEqual "Card value wrong" (cardValue $ Card Queen Diamonds) 10
testCardValue5 = TestCase $ assertEqual "Card value wrong" (cardValue $ Card Three Hearts) 3
testCardValue6 = TestCase $ assertEqual "Card value wrong" (cardValue $ Card Seven Hearts) 7

{-Tests if the toString function works correctly-}
testHandToString :: Test
testHandToString = TestCase $ assertEqual "Card value wrong" (handToString [Card Ace Spades, Card Five Hearts]) "Ace Of Spades\n     | Five Of Hearts"
testHandToString2 = TestCase $ assertEqual "Card value wrong" (handToString [Card Seven Diamonds, Card Eight Clubs]) "Seven Of Diamonds\n     | Eight Of Clubs"
testHandToString3 = TestCase $ assertEqual "Card value wrong" (handToString [Card Seven Diamonds, Card Eight Clubs, Card Ten Hearts]) "Seven Of Diamonds\n     | Eight Of Clubs\n     | Ten Of Hearts"

{-Tests if the hasAce works correctly-}
testHasAce :: Test
testHasAce = TestCase $ assertBool "Found an ace when it shouldn't" (hasAce [] == False)
testHasAce2 = TestCase $ assertBool "Couldn't find ace" (hasAce [Card Three Spades, Card Ace Hearts])
testHasAce3 = TestCase $ assertBool "Couldn't find ace" (hasAce [Card Three Spades, Card Ace Hearts, Card Five Clubs, Card Six Hearts])
testHasAce4 = TestCase $ assertBool "Couldn't find ace" (hasAce [Card Three Spades, Card Five Hearts, Card Ace Clubs, Card Six Hearts])

{-Tests if the hand containing an ace is added correctly -}
testCalculateAceHand :: Test
testCalculateAceHand = TestCase $ assertEqual "hand calculated wrong" (calculateAceHand [Card Ace Spades, Card Ace Hearts]) 12
testCalculateAceHand2 = TestCase $ assertEqual "hand calculated wrong" (calculateAceHand []) 0
testCalculateAceHand3 = TestCase $ assertEqual "hand calculated wrong" (calculateAceHand [Card Ace Spades, Card Ace Hearts, Card Six Hearts]) 18
testCalculateAceHand4 = TestCase $ assertEqual "hand calculated wrong" (calculateAceHand [Card Ace Spades, Card Ace Hearts, Card Ten Hearts, Card Nine Spades]) 21

tests :: Test
tests = TestList [TestLabel "test" testMakeDeck,
                  TestLabel "test" testRandomizeDeck,
                  TestLabel "test" testDrawCard,
                  TestLabel "test" testDrawCard2,
                  TestLabel "test" testCalculateHand,
                  TestLabel "test" testCalculateHand2,
                  TestLabel "test" testCalculateHand3,
                  TestLabel "test" testCalculateHand4,
                  TestLabel "test" testCalculateHand5,
                  TestLabel "test" testHandToString,
                  TestLabel "test" testHandToString2,
                  TestLabel "test" testHandToString3,
                  TestLabel "test" testHasAce,
                  TestLabel "test" testHasAce2,
                  TestLabel "test" testHasAce3,
                  TestLabel "test" testHasAce4,
                  TestLabel "test" testCalculateAceHand,
                  TestLabel "test" testCalculateAceHand2,
                  TestLabel "test" testCalculateAceHand3,
                  TestLabel "test" testCalculateAceHand4,
                  TestLabel "test" testCardValue,
                  TestLabel "test" testCardValue2,
                  TestLabel "test" testCardValue3,
                  TestLabel "test" testCardValue4,
                  TestLabel "test" testCardValue5,
                  TestLabel "test" testCardValue6]

main = do runTestTT tests