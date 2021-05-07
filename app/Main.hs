module Main where

import Deck
import Play

main :: IO ()
main = do
  putStrLn "Welcome to Blackjack Trainer!"
  numberOfDecks <- getNumberOfDecks
  theDeck <- shuffledDecks numberOfDecks
  money <- askForMoney
  putStrLn "Lets play Blackjack!"
  play theDeck money
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

-- | Ask for a number of decks until a valid number is given. (Only handles numbers tho)
askForMoney :: IO Int
askForMoney = do
    putStrLn "How much money would you like? (No Cents)"
    input <- getLine 
    let cashRequested = (read input :: Int)
    if 0 < cashRequested
        then return cashRequested
    else askForMoney