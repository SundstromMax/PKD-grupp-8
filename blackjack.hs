module Blackjack(main) where

import System.Exit

data Cardtypes = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show,Eq, Enum)
data Suits = Spades | Clubs | Diamonds | Hearts deriving(Show, Eq, Enum)
data Card = Card Cardtypes Suits deriving(Show, Eq)
type Deck = [Card] 
type Hand = [Card]

data GameState = GameState{
    deck :: Deck,
    playerHand :: Hand,
    dealerHand :: Hand
} deriving(Show)

main :: IO()
main = menu

menu :: IO()
menu = do
    putStrLn "Welcome to our blackjack game! \n Play \n Quit"
    answer <- getLine 
    let choice  | answer == "Play" = gameLoop initState
                | answer == "Quit" = exitSuccess
                | otherwise = menu
    choice      

initState :: GameState
initState = GameState {
    deck = makeDeck,
    playerHand = [],
    dealerHand = []
}

makeDeck :: Deck
makeDeck = [Card cardtypes suits | cardtypes <- [Two ..], suits <- [Spades ..]]

gameLoop :: GameState -> IO()
gameLoop gs = do
    putStrLn . show $ deck gs

dealCards :: GameState -> GameState
dealCards gs = gs

cardValue :: Cardtypes -> Int
cardValue Two = 2
cardValue Three = 3
cardValue Four = 4
cardValue Five = 5
cardValue Six = 6
cardValue Seven = 7
cardValue Eight = 8
cardValue Nine = 9
cardValue Ten = 10
cardValue Jack = 10
cardValue Queen = 10
cardValue King = 10
cardValue Ace = 11
