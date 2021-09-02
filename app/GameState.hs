module GameState where

import Deck

-- | Data structure for storing the game state
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

-- Here are some convenience getters and modifiers for the game state

-- | Setter for bet in GameState
setBet :: Int -> GameState -> GameState
setBet newBet oldState = 
    let
        oldMoney = money oldState
    in oldState { bet = newBet, money = oldMoney - newBet}

-- | Special case of the above to double the bet.
doubleBet :: GameState -> GameState
doubleBet oldState =
    let
        oldBet = bet oldState
        oldMoney = money oldState
        doubledBet = 2 * oldBet
    in oldState {bet = doubledBet, money = oldMoney - oldBet}

-- | Gets the hand that is currently active from the state.
activeHand :: GameState -> [Card]
activeHand state    | hand2Active state = playerHand2 state
                    | otherwise = playerHand1 state

-- | Clear player and dealer hands along with the bet
clearHandsAndBets :: GameState -> GameState
clearHandsAndBets oldState = oldState {playerHand1 = [], playerHand2 = [], dealerHand = [], bet = 0, 
    split = False, hand2Active = False}

-- | Set a hand2active to the given boolean value
setHand2Active :: Bool -> GameState -> GameState
setHand2Active value oldState = oldState {hand2Active = value}