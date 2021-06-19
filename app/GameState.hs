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

-- | Setter for bet in GameState
setBet :: Int -> GameState -> GameState
setBet newBet oldState = 
    let
        oldMoney = money oldState
    in oldState { bet = newBet, money = oldMoney - newBet}


-- | Gets the hand that is currently active from the state.
activeHand :: GameState -> [Card]
activeHand state    | hand2Active state = playerHand2 state
                    | otherwise = playerHand1 state