module Main where

main :: IO ()
main = do
  putStrLn "Welcome to BlackJack Trainer!"
  decks <- getNumberOfDecks
  putStrLn "Bye bye!"

getNumberOfDecks :: IO Int
getNumberOfDecks = do
    putStrLn "How many deck's would you like to play with? (1-8)"
    input <- getLine 
    let numberOfDecks = (read input :: Int)
    if 0 < numberOfDecks && numberOfDecks < 9
        then return numberOfDecks
    else getNumberOfDecks
