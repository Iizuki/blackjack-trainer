module Play (play) where

import Deck
import GameState
import Hand
import Action
import Text.SimpleTableGenerator
import Control.Monad.State
import Control.Monad (when)


-- | Initialize the actual game
play :: Deck -> Int -> IO ()
play deck money = do 
    let initialGameState = GameState deck money [] [] [] 0 False False :: GameState
    print "The game begins!"
    runStateT playRound initialGameState
    return ()


-- | Play the game one round at a time
playRound :: StateT GameState IO ()
playRound = do
    liftIO $ putStrLn "New Round!"
    placeBet
    draw1ForDealer  -- Initial hand for the dealer
    drawForPlayer 2 -- Intitial hand for the player
    playHand
    -- Play the other hand too if a split occured
    handWasSplit <- gets split
    -- TODO update active hand
    when handWasSplit playHand 
    playRound -- TODO some check to prevent infinite loop


-- | Play one hand, as in blackjack, splitting can result in player having multiple hands.
playHand :: StateT GameState IO ()
playHand = do
    printGameState
    playerAction <- chooseAction
    return ()


-- | Asks the player for his bet
placeBet :: StateT GameState IO ()
placeBet = do
    moneyLeft <- gets money
    liftIO $ print "Place your bet:"
    input <- liftIO getLine 
    let bet = (read input :: Int)
    if bet < 1 || bet > moneyLeft
        -- The bet was illegal
        then placeBet
    else 
        modify (setBet bet)



-- | Draws one card for the dealer.
draw1ForDealer :: (Monad m) => StateT GameState m ()
draw1ForDealer = do
    state <- get 
    let currentDeck = deck state
        currentDealerHand = dealerHand state
        (drawedCard, newDeck) = draw currentDeck 1
        newState = state { deck = newDeck, dealerHand = currentDealerHand ++ drawedCard}
    put newState


-- | Draw a number of cards to the player, typically 1 or 2 in blackjack.
drawForPlayer :: Monad m => Int -> StateT GameState m ()
drawForPlayer number = do
    state <- get
    let currentDeck = deck state
        hand2 = hand2Active state
        currentHand | hand2 = playerHand2 state
                    | otherwise = playerHand1 state
        (drawedCards, newDeck) = draw currentDeck number
        newState  | hand2 = state { deck = newDeck, playerHand2 = currentHand ++ drawedCards}
                  | otherwise = state { deck = newDeck, playerHand1 = currentHand ++ drawedCards}
    put newState


-- | Dropping the parenthesis
getPrettyHand :: [Card] -> String 
getPrettyHand hand = tail $ init $ show hand


-- | Prints the state of the game to the console in a table format.
printGameState :: StateT GameState IO ()
printGameState = do
    state <- get
    let cardsLeftInDeck = show $ length $ deck state :: String
        moneyLeft = show $ money state :: String
        bet' = show $ bet state
        dealerHand' = getPrettyHand $ dealerHand state :: String 
        playerHand = getPrettyHand $ activeHand state
        graphicalState =    [["Deck", "Money", "Bet", "Dealer Hand", "Your hand"], 
                            [cardsLeftInDeck, moneyLeft, bet', dealerHand', playerHand]] :: [[String]]
    liftIO $ putStrLn $ makeDefaultSimpleTable graphicalState