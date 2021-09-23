module Action where

import GameState
import Hand
import Control.Monad.State

-- | Possible player actions in the game
data Action = Stand | Hit | Double | Split deriving(Show, Read, Eq)
    

-- | Determines the list of legal actions in the current state of the game
legalActions :: GameState -> [Action]
legalActions state = 
    let addDouble = doublePossible state
        addSplit = splitPossible state
        addHit = hitPossible state
    in Stand : (maybeGameAction Hit addHit) ++ (maybeGameAction Split addSplit) ++ (maybeGameAction Double addDouble)

-- | Return a list that contains the action if 'yes' is true. 
--   Maybe monad would have been possible but concatenation is easier with lists.
maybeGameAction :: Action -> Bool -> [Action] 
maybeGameAction action yes 
    | yes       = [action]
    | otherwise = []

-- | Doubling is possible if hitting is possible, you can afford it and there are no splits.
doublePossible :: GameState -> Bool
doublePossible state =
    let handIsSplit = split state
        canHit = hitPossible state
    in
    if not canHit || handIsSplit -- No doubling together with splitting.
        then False
    else 
        let moneyLeft = money state
            currentBet = bet state
        in moneyLeft >= currentBet


splitPossible :: GameState -> Bool
splitPossible state =
    if split state 
        then False -- Splitting is allowed only once
    else
        let hand = activeHand state
            evaluatedHand = evaluateHand hand
        in evaluatedHand == Pair


hitPossible :: GameState -> Bool
hitPossible state = let
    hand = activeHand state 
    in sumCards hand <= 21

dropParenthesis :: String -> String 
dropParenthesis string = init $ tail string

-- | Asks the user for his next action
chooseAction :: StateT GameState IO Action
chooseAction = do
    state <- get
    let choices = legalActions state
        message = "Choose your action (" ++ dropParenthesis(show choices) ++ ")"
    liftIO $ putStrLn message
    input <- liftIO getLine
    let action = read input :: Action -- This will crash on invalid input but that's acceptable in this project.
    return action