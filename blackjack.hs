module Blackjack(main) where

data Cardtypes = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show,Eq, Enum)

main :: IO()
main = menu

menu :: IO()
menu = do
    putStrLn "Welcome to our blackjack game! \n Play \n Quit"
    answer <- getLine 
    let choice  | answer == "Play" = putStrLn "spelfunktion" {-TODO-}
                | answer == "Quit" = putStrLn "Quit this shit" {- TODO Quit funktion-}
                | otherwise = menu
    choice      


