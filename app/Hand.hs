module Hand where

import Deck
import GameState
import Data.Bool (Bool)
import Data.Int (Int)

-- | A player hand will be evaluated as one of these types
data Hand = Ordinary | Pair | Blackjack | Bust deriving(Eq)

-- | A fully evaluated hand. Contains the sum of the cards together with the blackjack hand status.
--   The main point of this is that a blackjack beats an ordinary 21.
data HandEvaluation = HandEvaluation {status :: Hand, cardsum :: Int}

-- | A result of comparing player hand to the dealer hand
data HandResult =  Lose | BustLose | Win | BlackjackWin | DealerBustWin | Draw deriving(Eq)

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

-- | Fully evaluate a hand. Returns the sum and the status.
fullyEvaluateHand :: [Card] -> HandEvaluation
fullyEvaluateHand hand = HandEvaluation (evaluateHand hand) (sumCards hand)

-- | Compare player hand to dealerhand
compareHands :: [Card] -- ^ Playerhand
    -> [Card] -- ^ Dealerhand
    -> HandResult 
compareHands playerHand dealerHand = let
    playerHandEvaluation = fullyEvaluateHand playerHand
    dealerHandEvaluation = fullyEvaluateHand dealerHand
    in compareEvaluatedHands playerHandEvaluation dealerHandEvaluation

-- | Compare fully evaluated hands
compareEvaluatedHands :: HandEvaluation -> HandEvaluation -> HandResult
compareEvaluatedHands playerHand dealerHand
    | status playerHand == Bust = Lose -- Busted player always loses

    | status playerHand == Blackjack
    , status dealerHand /= Blackjack = BlackjackWin -- Blackjack beats 21's composed in other ways

    | status dealerHand == Bust = DealerBustWin -- If dealer busts and player the player doesn't, he always wins. 
                                                -- Here this is also guaranteed not to be a blackjack win.

    -- The 'normal' wins and loses based on card sums.
    | cardsum playerHand > cardsum dealerHand = Win

    | cardsum playerHand < cardsum dealerHand = Lose

    | cardsum playerHand == cardsum dealerHand = Draw -- Blackjack draws should fall in here too

    | otherwise  = error "Hand evaluation failed" -- This should never happen