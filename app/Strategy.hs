{-|
    Description :   This module contains the Basic Strategy for blackjack, and the funtions to give feedback on the player's
                    actions. I.e. when a player chooses to Hit, we say that you should have in fact Doubled in that situation.
-}
module Strategy where

import Action
import GameState
import Hand
import Deck
import Data.Array.IArray


-- | Return the correct action in this game state. No action is returned if the correct action
--  is disabled for some reason.
correctAction :: GameState -> Maybe Action 
correctAction state = do
    let choices = legalActions state
        correctChoice = correctAction' state
    if correctChoice `elem` choices
        then Just correctChoice
    else Nothing -- There be situations where the optimal choice is not available, for example because the player
                 -- doesn't have the money to double. Here we make sure that the recommended action is also possible.


correctAction' :: GameState -> Action 
correctAction' state = let
    playerHand = activeHand state
    playerHandEvaluation = fullyEvaluateHand playerHand
    playerHandType = status playerHandEvaluation
    dealerCard = head $ dealerHand state -- There should be only the face card at this point.
    in case playerHandType of 
        Pair -> correctPairAction playerHand dealerCard
        _ -> correctPairAction playerHand dealerCard -- TODO: Just using the pair function for testing.


-- | Only call with a pair.
correctPairAction :: [Card] -> Card -> Action 
correctPairAction playerHand dealerCard =
    let dealerCardValue = if dealerCard == Ace then 11 else fromEnum dealerCard
        playerPair = head playerHand -- Assuming that the hand actually is a pair
        playerPairValue = if playerPair == Ace then 11 else fromEnum playerPair
    in pairs ! (playerPairValue, dealerCardValue)


--The following tables contain the basic strategy for BlackJack. 
--Source: https://en.wikipedia.org/wiki/Blackjack#Basic_strategy
--
--In these arrays the first coordinate corrresponds to the players hand, 
--and the second coordinate corresponds to the dealers face up card.

-- | Array contains the basic strategy for pairs. The first coordinate is the player's paired card
--   and the second coordinate is the dealers face card. Aces are represented as 11s.
pairs :: Array (Int, Int) Action
pairs = listArray ((2,2), (11,11)) 
    [Split, Split,  Split,  Split,  Split,  Split,  Hit,    Hit,    Hit,    Hit,
    Split,  Split,  Split,  Split,  Split,  Split,  Hit,    Hit,    Hit,    Hit,
    Hit,    Hit,    Hit,    Split,  Split,  Hit,    Hit,    Hit,    Hit,    Hit,
    Double, Double, Double, Double, Double, Double, Double, Double, Hit,    Hit,
    Split,  Split,  Split,  Split,  Split,  Hit,    Hit,    Hit,    Hit,    Hit,
    Split,  Split,  Split,  Split,  Split,  Split,  Hit,    Hit,    Hit,    Hit,
    Split,  Split,  Split,  Split,  Split,  Split,  Split,  Split,  Split,  Split,
    Split,  Split,  Split,  Split,  Split,  Stand,  Split,  Split,  Stand,  Stand,
    Stand,  Stand,  Stand,  Stand,  Stand,  Stand,  Stand,  Stand,  Stand,  Stand,
    Split,  Split,  Split,  Split,  Split,  Split,  Split,  Split,  Split,  Split]


-- | Array contains the basic strategy for soft hands. The first coordinate is the playerhand's soft total,
--   and the second coordinate is the dealers face card. Dealer aces are represented as 11s.
softTotals :: Array (Int, Int) Action 
softTotals = listArray ((13, 2), (20, 11))
    [Hit,   Hit,    Hit,    Double, Double, Hit,    Hit,    Hit,    Hit,    Hit,
    Hit,    Hit,    Hit,    Double, Double, Hit,    Hit,    Hit,    Hit,    Hit,
    Hit,    Hit,    Double, Double, Double, Hit,    Hit,    Hit,    Hit,    Hit,
    Hit,    Hit,    Double, Double, Double, Hit,    Hit,    Hit,    Hit,    Hit,
    Hit,    Double, Double, Double, Double, Hit,    Hit,    Hit,    Hit,    Hit,
    Double, Double, Double, Double, Double, Stand,  Stand,  Hit,    Hit,    Hit,
    Stand,  Stand,  Stand,  Stand,  Double, Stand,  Stand,  Stand,  Stand,  Stand,
    Stand,  Stand,  Stand,  Stand,  Stand,  Stand,  Stand,  Stand,  Stand,  Stand]