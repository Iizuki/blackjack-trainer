module Play (play) where

import Deck
import Hand
import Text.SimpleTableGenerator
import Control.Monad.State
import Control.Monad (when)

-- | Data structures for storing game state
data GameState = GameState {
    deck :: Deck,
    money :: Int,
    dealerHand :: [Card],
    playerHand1 :: [Card],
    playerHand2 :: [Card], -- Only used in case of a split
    bet :: Int,
    split :: Bool, -- A split has occured
    hand2Active :: Bool -- A split has occured and the second hand is active
}

-- | Possible player actions in the game
data GameAction = Stand | Hit | Double | Split

-- | Play the game!
play :: Deck -> Int -> IO ()
play deck money = do 
    let initialGameState = GameState deck money [] [] [] 0 False False :: GameState
    runStateT playRound initialGameState
    return ()


-- | Play one round
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

-- | Play one hand, as in blackjack, splitting can result in player having multiple hands.
playHand :: StateT GameState IO ()
playHand = do
    printGameState
    liftIO $ print "Choose your action"
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


-- | Determines the list of legal actions in the current state of the game
legalActions :: Monad m => StateT GameState m [GameAction]
legalActions = do
    addDouble <- doublePossible
    addSplit <- splitPossible
    addHit <- hitPossible
    return [] -- TODO


-- | Doubling is possible if you can afford it and there are no splits.
doublePossible :: Monad m => StateT GameState m Bool
doublePossible = do
    handIsSplit <- gets split
    if handIsSplit
        then return False -- No doubling together with splitting
        else do
            moneyLeft <- gets money
            currentBet <- gets bet
            return $ moneyLeft >= (2 * currentBet)


splitPossible :: Monad m => StateT GameState m Bool
splitPossible = do
    state <- get
    if split state 
        then return False -- Splitting is allowed only once
    else do
        let hand = activeHand state
            evaluatedHand = evaluateHand hand
        return $ evaluatedHand == Pair


hitPossible :: Monad m => StateT GameState m Bool
hitPossible = do
    hand <- gets activeHand
    return $ sumCards hand <= 21


-- | Gets the hand that is currently active from the state.
activeHand :: GameState -> [Card]
activeHand state    | hand2Active state = playerHand2 state
                    | otherwise = playerHand1 state


-- | Prints the state of the game to the console in a table format.
printGameState :: StateT GameState IO ()
printGameState = do
    state <- get
    let cardsLeftInDeck = show $ length $ deck state :: String
        moneyLeft = show $ money state :: String
        bet' = show $ bet state
        dealerHand' = tail $ init (show $ dealerHand state) -- Dropping the parenthesis
        graphicalState =    [["Deck", "Money", "Bet", "Dealer Hand"], 
                            [cardsLeftInDeck, moneyLeft, bet', dealerHand']]
    liftIO $ putStrLn $ makeDefaultSimpleTable graphicalState
    return ()