module Play (play) where

import Deck
import GameState
import Hand
import Action
import Text.SimpleTableGenerator
import Control.Monad.State
import Control.Monad
import Data.Bool (Bool)
import GHC.Base (Bool)


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
    when handWasSplit playSplitHand

    dealerPlays -- Dealer plays his turn (unless the player lost already)
    modify clearHandsAndBets -- Cleanup for the next round
    playRound -- TODO some check to prevent infinite loop


-- | Play one hand.
--   In blackjack splitting can result in player having multiple hands so this might be called several times.
playHand :: StateT GameState IO ()
playHand = do
    printGameState
    playerAction <- chooseAction
    executeAction playerAction
    if playerAction == Stand 
        then return ()
    else if playerAction == Double 
        then do
            printGameState
            return ()
        else do 
            state <- get
            let handSum = currentHandSum state
            if handSum < 21
                then playHand -- Let the player continue playing this hand if it's not 21 or bust
            else do 
                printGameState -- Otherwise just show what happened and move on
                return ()   


-- | Play the second hand when a split has occurred. Assumes that the first two cards are already drawn.
--   The actual playing is handled by the same function that played the first hand. This just sets things ready.
playSplitHand :: StateT GameState  IO ()
playSplitHand = liftIO (putStrLn "Playing the 2nd hand of the split now.")
    >> modify (setHand2Active True)
    >> playHand

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


-- | Determines whether the dealer needs to take his turn or is the round over already.
dealerNeedsToPlay :: GameState -> Bool
dealerNeedsToPlay state = not (hand1IsResolved && hand2IsResolved)
    where hand1IsResolved = handIsResolved $ playerHand1 state
          hand2IsResolved = not (split state) && handIsResolved (playerHand2 state)


-- | Dealer plays his turn if necessary
dealerPlays :: StateT GameState IO ()
dealerPlays = get >>= \state -> pure (dealerNeedsToPlay state)
    >>= \needToPlay -> when needToPlay dealerDrawsUntilDone
    >> liftIO (putStrLn "Dealer's turn:")
    >> printGameState

-- | Dealer draws cards until his score is at least 17
dealerDrawsUntilDone :: Monad m => StateT GameState m ()
dealerDrawsUntilDone = draw1ForDealer
    >> gets dealerHand
    >>= \hand -> pure (sumCards hand)
    >>= \cardSum -> when (cardSum < 17) dealerDrawsUntilDone -- Dealer doesn't hit on soft 17.


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
        currentHand = activeHand state
        (drawedCards, newDeck) = draw currentDeck number
        newState  | hand2       = state { deck = newDeck, playerHand2 = currentHand ++ drawedCards}
                  | otherwise   = state { deck = newDeck, playerHand1 = currentHand ++ drawedCards}
    put newState


-- | Executes the given player action.
--   This doesn't check the legality of the given action.
executeAction :: Monad m => Action -> StateT GameState m ()
executeAction Stand = return () -- No operation

executeAction Hit = drawForPlayer 1 -- Hitting just means drawing one more card.
     
executeAction Double = do -- Draw one card for the player and double the bet.
    drawForPlayer 1
    modify doubleBet

executeAction Split = do
    state <- get
    let
        currentHand = activeHand state
        newCurrentHand = head currentHand -- Keep the first card in the current hand
        newSplitHand = tail currentHand -- The other card goes to the split hand. Assuming hand size of 2 here.
        currentBet = bet state
        oldMoney = money state
        newMoney = oldMoney - currentBet -- The the current bet is subtracted from the money a second time, as the splithand
                                         -- implicityly receives the same bet.
        newState = state {playerHand1 = [newCurrentHand], playerHand2 = newSplitHand, money = newMoney}
    put newState -- This is not ready yet
    drawForPlayer 1 -- Draw a replacement card to the player's current hand
    -- Temporarily activate the split hand in order to draw a card to it.
    modify $ setHand2Active True 
    drawForPlayer 1
    modify $ setHand2Active False


-- Printing

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