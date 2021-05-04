module Main where

import Deck

main :: IO ()
main = do
  putStrLn "Welcome to BlackJack Trainer!"
  numberOfDecks <- getNumberOfDecks
  theDeck <- shuffledDecks numberOfDecks
  print theDeck -- Debugging
  putStrLn "Bye bye!"

-- | Ask for a number of decks until a valid number is given. (Only handles numbers tho)
getNumberOfDecks :: IO Int
getNumberOfDecks = do
    putStrLn "How many deck's would you like to play with? (1-8)"
    input <- getLine 
    let numberOfDecks = (read input :: Int)
    if 0 < numberOfDecks && numberOfDecks < 9
        then return numberOfDecks
    else getNumberOfDecks
