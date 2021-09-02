module Hand where

import Deck
import GameState
import Data.Bool (Bool)
import Data.Int (Int)

-- | A player hand will be evaluated as one of these types
data Hand = Ordinary | Pair | Blackjack | Bust deriving(Eq)

-- | Calculates the sum of card values according to the rules of blackjack.
sumCards :: [Card] -> Int
sumCards cards  = sum (map fromEnum cards) + aceBonus
    where aceBonus  | Ace `elem` cards
                    , sum (map fromEnum cards) < 12
                    = 10
                    | otherwise  
                    = 0


-- | Determine the type of the hand
evaluateHand :: [Card] -> Hand
evaluateHand [] = Ordinary
evaluateHand [x] = Ordinary
evaluateHand [x,y] 
    | fromEnum x == fromEnum y      = Pair -- All tens are treated the same. I.e. J & Q is a pair.
    | x == Ace, fromEnum y == 10    = Blackjack
    | y == Ace, fromEnum x == 10    = Blackjack
    | otherwise                     = Ordinary
evaluateHand threeOrMoreCards
    | cardSum <- sumCards threeOrMoreCards
    , cardSum < 22  = Ordinary
    | otherwise     = Bust


-- | Evaluate the currently selected hand
evaluateCurrentHand :: GameState -> Hand
evaluateCurrentHand state = evaluateHand $ activeHand state

-- | Sum of the current hand
currentHandSum :: GameState -> Int
currentHandSum state = sumCards $ activeHand state

-- | Tells whether the hand is resolved, i.e. the dealer doesn't need to play his hand after bust.
handIsResolved :: [Card] -> Bool
handIsResolved hand = evaluateHand hand == Bust