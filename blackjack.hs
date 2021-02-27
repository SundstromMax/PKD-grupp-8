module Blackjack where
import System.Random ( mkStdGen, Random(randomR), RandomGen, StdGen )
import System.Exit (exitSuccess)
import Data.Text     (pack, toUpper)
import Data.Function (on)
import Data.List     (sortBy)
import Test.HUnit ()
import Data.Map (Map, insert, elems, singleton, (!))

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
    putStrLn "\ESC[2JEnter Current time on format mdhms(Month, Day, Hour, Minute, Second"
    seed <- getLine 
    putStrLn "\ESC[2JWelcome to our blackjack game! \n Play \n Quit"
    answer <- getLine
    let choice  | toUpper (pack answer) == pack "PLAY" = gameStart $ initState $ read seed
                | toUpper (pack answer) == pack "QUIT" = exitSuccess
                | otherwise = menu
    choice

initState :: Int -> GameState
initState seed = GameState {
    deck = fst $ fisherYates (mkStdGen seed) makeDeck , --TODO: randomize order of deck
    playerHand = [],
    dealerHand = []
}

makeDeck :: Deck
makeDeck = [Card cardtypes suits | cardtypes <- [Two ..], suits <- [Spades ..]]

gameStart :: GameState -> IO()
gameStart gs = do
    
    hitOrStand $ drawCard $ drawCard gs

hitOrStand :: GameState -> IO ()
hitOrStand gs = do
    putStrLn $ "\ESC[2J" ++ "Player's hand: " ++ handToString (playerHand gs) ++ "\n Current value of hand: " ++ show (calculateHand $ playerHand gs)
    putStrLn $ "Dealer's hand: " ++ handToString [head $ dealerHand gs]
    putStrLn "Do you want to Hit or Stand?"
    answer <- getLine
    let choice  | toUpper (pack answer) == pack "HIT" = hit gs
                | toUpper (pack answer) == pack "STAND" = stand gs
                | otherwise = hitOrStand gs
    choice

hit :: GameState -> IO ()
hit gs
    | gameOver $ playerHand (playerDrawCard gs) = menu
    | otherwise = hitOrStand $ playerDrawCard gs

stand :: GameState -> IO ()
stand gs = do
    if calculateHand (dealerHand gs) > 17 then calculateResult gs else
        stand $ dealerDrawCard gs

calculateResult :: GameState -> IO ()
calculateResult gs = do
    if calculateHand (dealerHand gs) > 21 then win gs else
        if calculateHand (dealerHand gs) >= calculateHand (playerHand gs) then lose gs else win gs

win :: GameState -> IO ()
win gs = do
    putStrLn $ "\ESC[2J" ++ "Win\n Dealers hand: " ++ handToString (dealerHand gs) ++ "\n Value of dealers hand: " ++ show (calculateHand $ dealerHand gs)

lose :: GameState -> IO ()
lose gs = do
    putStrLn $ "\ESC[2J" ++ "Lose\n Dealers hand: " ++ handToString (dealerHand gs) ++ "\n Value of dealers hand: " ++ show (calculateHand $ dealerHand gs)

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

handToString :: Hand -> String
handToString (x:[]) = show x
handToString (x:xs) = show x ++ ", " ++ handToString xs

gameOver :: Hand -> Bool
gameOver hand = calculateHand hand > 21


-- Got from https://wiki.haskell.org/Random_shuffle
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)
-- Got from https://wiki.haskell.org/Random_shuffle