module Action where

import GameState
import Hand
import Control.Monad.State

-- | Possible player actions in the game
data Action = Stand | Hit | Double | Split deriving(Show, Read)
    

-- | Determines the list of legal actions in the current state of the game
legalActions :: Monad m => StateT GameState m [Action]
legalActions = do
    addDouble <- doublePossible
    addSplit <- splitPossible
    addHit <- hitPossible
    return $ Stand : (maybeGameAction Hit addHit) ++ (maybeGameAction Split addSplit) ++ (maybeGameAction Double addDouble)

maybeGameAction :: Action -> Bool -> [Action] 
maybeGameAction action yes 
    | yes       = [action]
    | otherwise = []

-- | Doubling is possible if hitting is possible, you can afford it and there are no splits.
doublePossible :: Monad m => StateT GameState m Bool
doublePossible = do
    handIsSplit <- gets split
    canHit <- hitPossible
    if not canHit || handIsSplit -- No doubling together with splitting.
        then return False
        else do
            moneyLeft <- gets money
            currentBet <- gets bet
            return $ moneyLeft >= currentBet


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

dropParenthesis :: String -> String 
dropParenthesis string = init $ tail string

-- | Asks the user for his next action
chooseAction :: StateT GameState IO Action
chooseAction = do
    choices <- legalActions
    let message = "Choose your action (" ++ dropParenthesis(show choices) ++ ")"
    liftIO $ print message
    input <- liftIO getLine
    let action = read input :: Action -- Thi will crash on invalid input but that's acceptable in this project.
    liftIO $ print $ "You chose " ++ show action -- Debug
    return action