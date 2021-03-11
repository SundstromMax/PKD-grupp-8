module Blackjack where
import System.Random ( mkStdGen, Random(randomR), RandomGen, StdGen )
import System.Exit (exitSuccess)
import Data.Text     (pack, toUpper)
import Data.List     (sortBy)
import Data.Map (Map, insert, elems, singleton, (!))

{-Data structure used for representing the values a playing card can have-}
data Cardtypes = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show,Eq, Enum)

{-Data structure used for representing the suits-}
data Suits = Spades | Clubs | Diamonds | Hearts deriving(Show, Eq, Enum)

{-Data structure to represent a card -}
data Card = Card Cardtypes Suits deriving(Eq)

{-Data type to represent a Deck -}
type Deck = [Card]

{-Data type to represent a Hand -}
type Hand = [Card]

{- Data structure to represent the game state
   Variables need to be updated during a game of blackjack to keep track of who has which cards. 
-}
data GameState = GameState{
    deck :: Deck,
    playerHand :: Hand,
    dealerHand :: Hand
} deriving(Show)

instance Show Card where
   show (Card cardtypes suits) = show cardtypes ++ " Of " ++ show suits


{- main
   Starts the chain of functions calling each other by calling the menu function
   SIDE EFFECTS: Prints out messages to terminal and waits for input from user.
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
    putStrLn "\ESC[2J"
    putStrLn "Enter Current time on format mmddhhmm(Month, Day, Hour, Minute)"
    seed <- getLine 
    putStrLn "\ESC[2J"
    putStrLn "-----------------------------\n"
    printCard
    putStrLn ""
    putStrLn "    Welcome to blackjack! \n           Play \n           Quit\n"
    putStrLn "-----------------------------\n"
    answer <- getLine
    putStrLn $ "\ESC[2J"
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
   EXAMPLE: makeDeck == "[Card Two Spades,Card Two Clubs,Card Two Diamonds,Card Two Hearts,Card Three Spades,Card Three Clubs,Card Three Diamonds,Card Three Hearts,
   Card Four Spades,Card Four Clubs,Card Four Diamonds,Card Four Hearts,Card Five Spades,Card Five Clubs,Card Five Diamonds,Card Five Hearts,
   Card Six Spades,Card Six Clubs,Card Six Diamonds,Card Six Hearts,Card Seven Spades,Card Seven Clubs,Card Seven Diamonds,Card Seven Hearts,
   Card Eight Spades,Card Eight Clubs,Card Eight Diamonds,Card Eight Hearts,Card Nine Spades,Card Nine Clubs,Card Nine Diamonds,Card Nine Hearts,
   Card Ten Spades,Card Ten Clubs,Card Ten Diamonds,Card Ten Hearts,Card Jack Spades,Card Jack Clubs,Card Jack Diamonds,Card Jack Hearts,
   Card Queen Spades,Card Queen Clubs,Card Queen Diamonds,Card Queen Hearts,Card King Spades,Card King Clubs,Card King Diamonds,Card King Hearts,
   Card Ace Spades,Card Ace Clubs,Card Ace Diamonds,Card Ace Hearts]" 
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
    putStrLn "----------------------------------------------\n"
    putStrLn "    Dealer's hand:" 
    putStrLn $ "     | " ++ handToString [head $ dealerHand gs]
    putStrLn $ "     Current value of dealer's hand is " ++ show (cardValue $ head $ dealerHand gs)
    putStrLn ""
    putStrLn "    Your hand:"
    putStrLn $ "     | " ++ handToString (playerHand gs)
    putStrLn $ "     Current value of your hand is " ++ show (calculateAceHand $ playerHand gs)
    putStrLn ""
    putStrLn "         Do you want to Hit or Stand?\n"
    putStrLn "----------------------------------------------"
    answer <- getLine
    putStrLn $ "\ESC[2J"
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
hit gs = do
    putStrLn $ "     You hit and draw " ++ handToString [head (deck gs)]
    putStrLn ""
    if gameOver $ playerHand (playerDrawCard gs) 
       then lose $ playerDrawCard gs
       else hitOrStand $ playerDrawCard gs

{- stand gamestate
   Enabling the player to stand
   SIDE EFFECTS: Prints out messages to terminal and waits for input from user.
   EXAMPLE: stand gamestate == prints messages in the terminal and waits for user input
-}
stand :: GameState -> IO ()
stand gs = do
    if calculateAceHand (dealerHand gs) > 17 then calculateResult gs else
        stand $ dealerDrawCard gs

{- calculateResult gamestate
   Calculates if the dealer or player has won.
   SIDE EFFECTS: Prints out messages to terminal and waits for input from user.
   EXAMPLE: calculateResult gamestate == prints messages in the terminal and waits for user input     
-}
calculateResult :: GameState -> IO ()
calculateResult gs = do
    if gameOver(dealerHand gs) then win gs else
        if calculateAceHand (dealerHand gs) >= calculateAceHand (playerHand gs) then lose gs else win gs

{- win gamestate
   Prints the winner interface
   SIDE EFFECTS: Prints out messages to terminal and waits for input from user.
   EXAMPLE: win gamestate == prints messages in the terminal and waits for user input
-}
win :: GameState -> IO ()
win gs = do
    putStrLn "----------------------------------------------\n"
    putStrLn "                  You win!"
    putStrLn ""
    putStrLn "    Dealer's hand:" 
    putStrLn $ "     | " ++ handToString (dealerHand gs)
    putStrLn $ "     Value of dealer's hand is " ++ show (calculateAceHand $ dealerHand gs)
    putStrLn ""
    putStrLn "    Your hand:"
    putStrLn $ "     | " ++ handToString (playerHand gs)
    putStrLn $ "     Value of your hand is " ++ show (calculateAceHand $ playerHand gs)
    putStrLn ""
    putStrLn "       Type anything to go back to menu"
    putStrLn ""
    putStrLn "----------------------------------------------"
    answer <- getLine
    putStrLn $ "\ESC[2J"
    menu
    

{- lose gamestate
   Prints the loser interface
   SIDE EFFECTS: Prints out messages to terminal and waits for input from user.
   EXAMPLE: lose gamestate == prints messages in the terminal and waits for user input  
-}
lose :: GameState -> IO ()
lose gs = do
    putStrLn "----------------------------------------------\n"
    putStrLn "                  You lose!"
    putStrLn ""
    putStrLn "    Dealer's hand:" 
    putStrLn $ "     | " ++ handToString (dealerHand gs)
    putStrLn $ "     Value of dealer's hand is " ++ show (calculateAceHand $ dealerHand gs)
    putStrLn ""
    putStrLn "    Your hand:"
    putStrLn $ "     | " ++ handToString (playerHand gs)
    putStrLn $ "     Value of your hand is " ++ show (calculateAceHand $ playerHand gs)
    putStrLn ""
    putStrLn "       Type anything to go back to menu"
    putStrLn ""
    putStrLn "----------------------------------------------"
    answer <- getLine
    putStrLn $ "\ESC[2J"
    menu

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
             calculateHand [Card Two Spades,Card Two Clubs] == 4
             calculateHand [Card King Spades, Card King Clubs, Card King Diamonds] == 30        
-}
calculateHand :: Hand -> Int
--Variant: length hand 
calculateHand [] = 0
calculateHand (x:[]) = cardValue x
calculateHand (x:xs) = cardValue x + calculateHand xs

{- hasAce hand
   Checks if "hand" contains an Ace
   RETURNS: A boolean depending on if "hand" contains an ace
   EXAMPLES: hasAce [Card Two Spades,Card Two Clubs] == False
             hasAce [Card Ace Spades,Card Two Clubs] == True
             hasAce [Card King Spades,Card King Clubs, Card Ace Diamonds, Card Two Spades] == True         
-}
hasAce :: Hand -> Bool
--Variant: length hand
hasAce [] = False 
hasAce ((Card Ace _): _) = True 
hasAce (x:xs) = hasAce xs

{- calculateAceHand hand
   Checks if the Aces should have the value 1 or 11
   RETURNS: The integer value of "hand" in regard to the Aces.
   EXAMPLES: calculateAceHand [] == 0
             calculateAceHand [Card Ace Spades, Card Ace Hearts] == 12
             calculateAceHand [Card Ace Spades, Card King Hearts, Card King Spades] == 21
             calculateAceHand [Card Two Spades, Card Three Hearts, Card Ace Hearts] == 16     
-}
calculateAceHand :: Hand -> Int
calculateAceHand [] = 0
calculateAceHand hand = if hasAce hand && (calculateHand hand + 10 <= 21) then calculateHand hand + 10 else calculateHand hand

{- cardValue card
   Gives the card a integer value.
   RETURNS: The integer value of "card"
   EXAMPLES: cardValue (Card Two Spades) == 2
             cardValue (Card Three Clubs) == 3
             cardValue (Card Ten Diamonds) == 10
             cardValue (Card Ace Diamonds) == 1
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
cardValue (Card Ace _) = 1

{- handToString hand
   Changes the "hand" to a printable string
   PRE: hand != []
   RETURNS: A string representation of hand
   EXAMPLES: handToString [Two Of Spades] == "Two Of Spades"
             handToString [Two Of Spades,Two Of Clubs] == "Two Of Spades, Two Of Clubs"
             handToString [Two Of Spades,Two Of Clubs,King Of Spades] == "Two Of Spades, Two Of Clubs, King Of Spades"              
-}
handToString :: Hand -> String
--Variant: length hand
handToString (x:[]) = show x
handToString (x:xs) = show x ++ "\n     | " ++ handToString xs

-- Got from https://wiki.haskell.org/Random_shuffle
{- fisherYatesStep
   Help function for fisherYates              
-}
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

{- fisherYates randomGen list
   Randomly shuffles a list
   RETURNS: A tuple where the first element is a randomly shuffled version of the original list and the second is a random generator
   EXAMPLES: fisherYates (mkStdGen 1) [1, 2, 3, 4, 5] == ([1,4,5,3,2],879767458 1872071452)
             fisherYates (mkStdGen 2) ["a", "b", "c", "d", "e"] == (["c","e","d","a","b"],1319651187 1872071452)
             fisherYates (mkStdGen 3) [True, False, True, False, True] == ([True,False,True,True,False],1759534916 1872071452)             
-}
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
   EXAMPLES: gameOver [Card King Spades, Card King Clubs, Card King Diamonds] == True
             gameOver [Card Two Spades, Card Two Clubs] == False
             gameOver [Card Eight Clubs, Card Two Spades,Card Six Spades] == False
-}
gameOver :: Hand -> Bool
gameOver hand = calculateAceHand hand > 21


{- blackJack gamestate
   Checks if player has blackjack
   SIDE EFFECT: Prints out messagees to terminal and waits for input from user.   
   EXAMPLES: blackjack gamestate == prints messages in the terminal and waits for user input
-}
blackJack :: GameState -> IO()
blackJack gs = do 
   if calculateAceHand (playerHand gs) == 21
   then win gs 
   else hitOrStand gs

printCard :: IO()
printCard = do putStrLn "      ________________\n     |                |\n     | A              |\n     |                |\n     |                |\n     |                |\n     |                |\n     |       <3       |\n     |                |\n     |                |\n     |                |\n     |                |\n     |                |\n     |              A |\n     |________________|"

{- 
 ________________
|                |
| A              |
|                |
|                |
|                |
|                |
|       <3       |
|                |
|                |
|                |
|                |
|                |
|              A |
|________________|
-}