module Blackjack(main) where

import System.Random
import System.Exit

data Cardtypes = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show,Eq, Enum)
data Suits = Spades | Clubs | Diamonds | Hearts deriving(Show, Eq, Enum)
data Card = Card Cardtypes Suits deriving(Eq)
type Deck = [Card] 
type Hand = [Card]

data GameState = GameState{
    deck :: Deck,
    playerHand :: Hand,
    dealerHand :: Hand
} deriving(Show)

instance Show Card where
    show (Card cardtypes suits) = show cardtypes ++ " Of " ++ show suits

main :: IO()
main = menu

menu :: IO()
menu = do
    putStrLn "Welcome to our blackjack game! \n Play \n Quit"
    answer <- getLine 
    let choice  | answer == "Play" = gameStart initState
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

gameStart :: GameState -> IO()
gameStart gs = do
    print . show $ deck gs
    if null(deck gs) then menu else gameStart $ drawCard gs

drawCard :: GameState -> GameState
drawCard gs = dealerDrawCard $ playerDrawCard gs

playerDrawCard :: GameState -> GameState
playerDrawCard gs = gs { playerHand = playerHand gs ++ [head $ deck gs], deck = tail $ deck gs}

dealerDrawCard :: GameState -> GameState
dealerDrawCard gs = gs { dealerHand = dealerHand gs ++ [head $ deck gs], deck = tail $ deck gs}

calculateHand :: Hand -> Int
calculateHand [] = 0
calculateHand (x:[]) = cardValue x  
calculateHand (x:xs) = cardValue x + calculateHand xs

cardValue :: Card -> Int
cardValue (Card Two _) = 2
cardValue (Card Three _) = 3
cardValue (Card Four _) = 4
cardValue (Card Five _) = 5
cardValue (Card Six _) = 6
cardValue (Card Seven _) = 7
cardValue (Card Eight _) = 8
cardValue (Card Nine _) = 9
cardValue (Card Ten  _) = 10
cardValue (Card Jack _) = 10
cardValue (Card Queen _) = 10
cardValue (Card King _) = 10
cardValue (Card Ace _) = 11

gameOver :: Hand -> Bool
gameOver hand = calculateHand hand > 21