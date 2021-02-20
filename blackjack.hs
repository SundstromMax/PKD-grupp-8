module Blackjack(main) where

import System.Random
import System.Exit
import Data.Text as T

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
    putStrLn "\ESC[2JWelcome to our blackjack game! \n Play \n Quit"
    --putStrLn "Welcome to our blackjack game! \n Play \n Quit"
    answer <- getLine 
    let choice  | toUpper (pack answer) == pack "PLAY" = gameStart initState
                | toUpper (pack answer) == pack "QUIT" = exitSuccess
                | otherwise = menu
    choice      

initState :: GameState
initState = GameState {
    deck = makeDeck, --TODO: randomize order of deck
    playerHand = [],
    dealerHand = []
}

makeDeck :: Deck
makeDeck = [Card cardtypes suits | cardtypes <- [Two ..], suits <- [Spades ..]]

gameStart :: GameState -> IO()
gameStart gs = do
    if Prelude.null(deck gs) then menu else hitOrStand $ drawCard $ drawCard gs

hitOrStand :: GameState -> IO ()
hitOrStand gs = do
    putStrLn $ "\ESC[2J" ++ show (playerHand gs) ++ " " ++ show (calculateHand $ playerHand gs)
    print . show $ Prelude.head $ dealerHand gs
    putStrLn "Do you want to Hit or Stand?"
    answer <- getLine
    let choice  | toUpper (pack answer) == pack "HIT" = hit gs
                | toUpper (pack answer) == pack "STAND" = stand gs
                | otherwise = menu
    choice 

hit :: GameState -> IO ()
hit gs  
    | gameOver $ playerHand (playerDrawCard gs) = menu
    | otherwise = hitOrStand $ playerDrawCard gs

stand :: GameState -> IO ()
stand gs = do
    print . calculateHand $ playerHand gs

drawCard :: GameState -> GameState
drawCard gs = dealerDrawCard $ playerDrawCard gs

playerDrawCard :: GameState -> GameState
playerDrawCard gs = gs { playerHand = playerHand gs ++ [Prelude.head $ deck gs], deck = Prelude.tail $ deck gs}

dealerDrawCard :: GameState -> GameState
dealerDrawCard gs = gs { dealerHand = dealerHand gs ++ [Prelude.head $ deck gs], deck = Prelude.tail $ deck gs}

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