module Play (play) where

import Deck
import Text.SimpleTableGenerator
import Control.Monad.State
import Control.Lens

-- Data structures for storing game state
data GameState = GameState {
    deck :: Deck,
    money :: Int,
    dealerHand :: [Card],
    playerHand1 :: [Card],
    playerHand2 :: [Card], -- Only used in case of a split
    bet :: Int
}

-- | Play the game!
play :: Deck -> Int -> IO ()
play deck money = do 
    let initialGameState = GameState deck money [] [] [] 0 :: GameState
    runStateT playRound initialGameState
    return ()


-- | Play one round
playRound :: StateT GameState IO ()
playRound = do
    liftIO $ putStrLn "New Round!"
    placeBet
    draw1ForDealer
    printGameState
    return ()

-- | Play one hand, as in blackjack, splitting can result in player having multiple hands.
playHand :: StateT GameState IO ()
playHand = do
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


-- | Prints the state of the game to the console in a table format.
printGameState :: StateT GameState IO ()
printGameState = do
    state <- get
    let cardsLeftInDeck = show $ length $ deck state :: String
        moneyLeft = show $ money state :: String
        bet' = show $ bet state
        graphicalState =    [["Deck", "Money", "Bet", "Dealer Hand"], 
                            [cardsLeftInDeck, moneyLeft, bet']]
    liftIO $ putStrLn $ makeDefaultSimpleTable graphicalState
    return ()

-- | Setter for bet in GameState
setBet :: Int -> GameState -> GameState
setBet newBet oldState = 
    let
        oldMoney = money oldState
    in oldState { bet = newBet, money = oldMoney - newBet}

-- | Draws one card for the dealer.
draw1ForDealer :: (Monad m) => StateT GameState m ()
draw1ForDealer = do
    state <- get 
    let currentDeck = deck state
        currentDealerHand = dealerHand state
        (newDeck, drawedCard) = draw currentDeck 1
        newState = state { deck = newDeck, dealerHand = currentDealerHand ++ drawedCard}
    put newState