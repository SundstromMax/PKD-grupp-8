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

{- main
   TODO TODO TODO TODO TODO TODO TODO
-}
main :: IO()
main = menu

{- menu
   Prints the main menu where user can type in input depending on what they want to do.
   SIDE EFFECT: Prints out messages to terminal and waits for input from user.
   EXAMPLES: menu == prints messages in the terminal and waits for user input.
-}
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

{- initState
   Initialize the gamestate variables
   RETURNS: The updated gamestate
   EXAMPLES: initstate == GameState {deck = makeDeck,playerHand = [],dealerHand = []} 
-}
initState :: Int -> GameState
initState seed = GameState {
    deck = fst $ fisherYates (mkStdGen seed) makeDeck,
    playerHand = [],
    dealerHand = []
}
{- makeDeck
   Creates a deck of cards.
   RETURNS: a "deck"
   EXAMPLE: makeDeck == "[Two Of Spades,Two Of Clubs,Two Of Diamonds,Two Of Hearts,Three Of Spades,Three Of Clubs,Three Of Diamonds,Three Of Hearts,
   Four Of Spades,Four Of Clubs,Four Of Diamonds,Four Of Hearts,Five Of Spades,Five Of Clubs,Five Of Diamonds,Five Of Hearts,
   Six Of Spades,Six Of Clubs,Six Of Diamonds,Six Of Hearts,Seven Of Spades,Seven Of Clubs,Seven Of Diamonds,Seven Of Hearts,
   Eight Of Spades,Eight Of Clubs,Eight Of Diamonds,Eight Of Hearts,Nine Of Spades,Nine Of Clubs,Nine Of Diamonds,Nine Of Hearts,
   Ten Of Spades,Ten Of Clubs,Ten Of Diamonds,Ten Of Hearts,Jack Of Spades,Jack Of Clubs,Jack Of Diamonds,Jack Of Hearts,
   Queen Of Spades,Queen Of Clubs,Queen Of Diamonds,Queen Of Hearts,King Of Spades,King Of Clubs,King Of Diamonds,King Of Hearts,
   Ace Of Spades,Ace Of Clubs,Ace Of Diamonds,Ace Of Hearts]" 
-}
makeDeck :: Deck
makeDeck = [Card cardtypes suits | cardtypes <- [Two ..], suits <- [Spades ..]]

{- gameStart gamestate
   Starts a game of blackjack
   SIDE EFFECTS: Prints out messages to terminal and waits for input from user.
   EXAMPLE: gameStart gamestate == prints messages in the terminal and waits for user input
-}
gameStart :: GameState -> IO()
gameStart gs = do
    
    blackJack $ drawCard $ drawCard gs

{- hitOrStand gamestate
   The interface where the user gets to decide if they want to hit or stand
   SIDE EFFECTS: Prints out messages to terminal and waits for input from user.      
   EXAMPLE: hitOrStand gamestate == prints messages in the terminal and waits for user input
-}
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

{- hit gamestate
   Gives the player a new card from the deck and checks if the player busts
   SIDE EFFECTS: Prints out messages to terminal and waits for input from user.
   EXAMPLE: hit gamestate == prints messages in the terminal and waits for user input     
-}
hit :: GameState -> IO ()
hit gs
    | gameOver $ playerHand (playerDrawCard gs) = lose gs
    | otherwise = hitOrStand $ playerDrawCard gs

{- stand gamestate
   Enabling the player to stand
   SIDE EFFECTS: Prints out messages to terminal and waits for input from user.
   EXAMPLE: stand gamestate == prints messages in the terminal and waits for user input
-}
stand :: GameState -> IO ()
stand gs = do
    if calculateHand (dealerHand gs) > 17 then calculateResult gs else
        stand $ dealerDrawCard gs

{- calculateResult gamestate
   Calculates if the dealer or player has won.
   SIDE EFFECTS: Prints out messages to terminal and waits for input from user.
   EXAMPLE: calculateResult gamestate == prints messages in the terminal and waits for user input     
-}
calculateResult :: GameState -> IO ()
calculateResult gs = do
    if calculateHand (dealerHand gs) > 21 then win gs else
        if calculateHand (dealerHand gs) >= calculateHand (playerHand gs) then lose gs else win gs

{- win gamestate
   Prints the winner interface
   SIDE EFFECTS: Prints out messages to terminal and waits for input from user.
   EXAMPLE: win gamestate == prints messages in the terminal and waits for user input
-}
win :: GameState -> IO ()
win gs = do
    putStrLn $ "\ESC[2J" ++ "Win\n Dealers hand: " ++ handToString (dealerHand gs) ++ "\n Value of dealers hand: " ++ show (calculateHand $ dealerHand gs)

{- lose gamestate
   Prints the loser interface
   SIDE EFFECTS: Prints out messages to terminal and waits for input from user.
   EXAMPLE: lose gamestate == prints messages in the terminal and waits for user input  
-}
lose :: GameState -> IO ()
lose gs = do
    putStrLn $ "\ESC[2J" ++ "Lose\n Dealers hand: " ++ handToString (dealerHand gs) ++ "\n Value of dealers hand: " ++ show (calculateHand $ dealerHand gs)

{-drawCard gamestate
  2 cards is removed from "deck" and "dealerHand" and "playerHand" recieves one each.
  PRE: deck != []
  RETURNS: The updated gamestate
  EXAMPLES: drawCard gs == gs
-}
drawCard :: GameState -> GameState
drawCard gs = dealerDrawCard $ playerDrawCard gs

{-playerDrawCard gamestate
  Removes one card from "deck" and gives it to "playerHand"
  PRE: deck != []
  RETURNS: The updated gamestate
  EXAMPLES: playerDrawCard gs == gs
-}
playerDrawCard :: GameState -> GameState
playerDrawCard gs = gs { playerHand = playerHand gs ++ [head $ deck gs], deck = tail $ deck gs}

{-dealerDrawCard gamestate
  Removes one card from "deck" and gives it to "dealerhand"
  PRE: deck != []
  RETURNS: The updated gamestate
  EXAMPLES: dealerDrawCard gs == gs
-}
dealerDrawCard :: GameState -> GameState
dealerDrawCard gs = gs { dealerHand = dealerHand gs ++ [head $ deck gs], deck = tail $ deck gs}

{- calculateHand hand
   Calculates the value of "hand"
   RETURNS: The integer value of "hand"
   EXAMPLES: calculateHand [] == 0
             calculateHand [Two Of Spades,Two Of Clubs] == 4
             calculateHand [King Of Spades,King Of Clubs,King Of Diamonds] == 30
             
-}
calculateHand :: Hand -> Int
calculateHand [] = 0
calculateHand (x:[]) = cardValue x
calculateHand (x:xs) = cardValue x + calculateHand xs

{- cardValue card
   Gives the card a integer value.
   RETURNS: The integer value of "card"
   EXAMPLES: cardValue (Two Of Spades) == 2
             cardValue (Three Of Clubs) == 3
             cardValue (Ten Of Diamonds) == 10
             cardValue (Ace Of Diamonds) == 11
-}
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

{- handToString hand
   Changes the "hand" to a printable string
   PRE: hand != []
   RETURNS: A string representation of hand
   EXAMPLES: handToString [Two Of Spades] == "Two Of Spades"
             handToString [Two Of Spades,Two Of Clubs] == "Two Of Spades, Two Of Clubs"
             handToString [Two Of Spades,Two Of Clubs,King Of Spades] == "Two Of Spades, Two Of Clubs, King Of Spades"              
-}
handToString :: Hand -> String
handToString (x:[]) = show x
handToString (x:xs) = show x ++ ", " ++ handToString xs

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

{- gameOver hand
   Updates bool if hand limit is reached
   RETURNS: A boolean that depends on the value of "hand"    
   EXAMPLES: gameOver [King Of Spades,King Of Clubs,King Of Diamonds] == True
             gameOver [Two Of Spades,Two Of Clubs] == False
             gameOver [Eight Of Clubs,Two Of Spades,Six Of Spades] == False
-}
gameOver :: Hand -> Bool
gameOver hand = calculateHand hand > 21


{- blackJack gamestate
   Checks if player has blackjack
   SIDE EFFECT: Prints out messagees to terminal and waits for input from user.   
   EXAMPLES: blackjack gamestate == prints messages in the terminal and waits for user input
-}
blackJack :: GameState -> IO()
blackJack gs = do 
   if (calculateHand (playerHand gs) == 21)
   then win gs 
   else hitOrStand gs

printCard :: Card -> IO()
printCard (Card value suits) = do putStrLn " ________________\n|suits           |\n|                |\n|                |\n|                |\n|                |\n|                |\n|                |\n|     value      |\n|                |\n|                |\n|                |\n|                |\n|                |\n|                |\n|           suits|\n|________________|"

{- 
 _________________
|suits           |
|                |
|                |
|                |
|                |
|                |
|                |
|     value      |
|                |
|                |
|                |
|                |
|                |
|                |
|           suits|
|________________|
-}