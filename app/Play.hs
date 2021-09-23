module Play (play) where

import Deck
import GameState
import Hand
import Action
import Text.SimpleTableGenerator -- Used for printing the game state
import Control.Monad.State
import Data.Bool (Bool)
import Strategy (correctAction)


-- | Initialize the actual game
play :: Deck -> Int -> IO ()
play deck money = do 
    let initialGameState = GameState deck [] money [] [] [] 0 False False :: GameState
    print "The game begins!"
    runStateT playRound initialGameState
    return ()


-- | Play the game one round at a time
playRound :: StateT GameState IO ()
playRound = do
    liftIO $ putStrLn "New Round!"
    printGameState
    placeBet
    draw1ForDealer  -- Initial hand for the dealer
    drawForPlayer 2 -- Intitial hand for the player
    playHand
    handWasSplit <- gets split
    when handWasSplit playSplitHand -- Play the other hand too if a split occured 
    dealerPlays -- Dealer plays his turn (unless the player lost already)
    resolveRound -- Resolve the round outcome and pay winnings
    modify clearHandsAndBets -- Cleanup for the next round
    moneyLeft <- gets money
    if moneyLeft > 0 
        then playRound -- There's still money left, play another round
    else 
        liftIO $ putStrLn "You wen't broke, game over."


-- | Play one hand.
--   In blackjack splitting can result in player having multiple hands so this might be called several times.
playHand :: StateT GameState IO ()
playHand = do
    state <- get
    printGameState
    playerAction <- chooseAction
    -- Analyse whether the choice was correct
    let whatYouShouldHaveChosen = correctAction state
    liftIO $ printChoiceFeedback playerAction whatYouShouldHaveChosen
    -- Actually excecute the chosen action
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
playSplitHand = liftIO (putStrLn "Playing the 2nd hand of the split now:")
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
        modify $ setBet bet


-- | Determines whether the dealer needs to take his turn or is the round over already.
dealerNeedsToPlay :: GameState -> Bool
dealerNeedsToPlay state = not (hand1IsResolved && hand2IsResolved)
    where hand1IsResolved = handIsResolved $ playerHand1 state
          hand2IsResolved = not (split state) || handIsResolved (playerHand2 state)


-- | Dealer plays his turn if necessary
dealerPlays :: StateT GameState IO ()
dealerPlays = get >>= \state -> pure (dealerNeedsToPlay state)
    >>= \needToPlay -> when needToPlay ( dealerDrawsUntilDone
        >> liftIO (putStrLn "Dealer's turn:")
        >> printGameState)

-- | Dealer draws cards until his score is at least 17
dealerDrawsUntilDone :: StateT GameState IO ()
dealerDrawsUntilDone = draw1ForDealer
    >> gets dealerHand
    >>= \hand -> pure (sumCards hand)
    >>= \cardSum -> when (cardSum < 17) dealerDrawsUntilDone -- Dealer doesn't hit on soft 17.


-- | Draws one card for the dealer.
draw1ForDealer :: StateT GameState IO ()
draw1ForDealer = do
    drawedCard <- drawAndShuffleIfNecessary 1
    state <- get 
    let currentDealerHand = dealerHand state
        newState = state {dealerHand = currentDealerHand ++ drawedCard}
    put newState


-- | Draw a number of cards to the player, typically 1 or 2 in blackjack.
drawForPlayer :: Int -> StateT GameState IO ()
drawForPlayer number = do
    drawedCards <- drawAndShuffleIfNecessary number
    state <- get
    let hand2 = hand2Active state
        currentHand = activeHand state
        newState  | hand2       = state { playerHand2 = currentHand ++ drawedCards}
                  | otherwise   = state { playerHand1 = currentHand ++ drawedCards}
    put newState


-- | Draw a number of cards from the current deck. The drawed cards are returned.
--   Don't call this directly. Use drawAndShuffleIfNecessary instead.
drawFromCurrentDeck :: Monad m => Int -> StateT GameState m [Card]
drawFromCurrentDeck cardsToDraw = do
    state <- get
    let currentDeck = deck state
        (drawedCards, newDeck) = draw currentDeck cardsToDraw
        newState = state { deck = newDeck}
    put newState
    return drawedCards


-- | Draw a number of cards and shuffle the discardpile back into the deck if there are not enough cards to make the draw.
drawAndShuffleIfNecessary :: Int -> StateT GameState IO [Card]
drawAndShuffleIfNecessary cardsToDraw = do
    state <- get 
    let currentDeck = deck state
    if length currentDeck >= cardsToDraw
        then drawFromCurrentDeck cardsToDraw
    else do
        liftIO $ putStrLn "Shuffling discard pile back into the deck."
        let cardsToShuffle = currentDeck ++ discardPile state
        newDeck <- lift $ shuffleCards cardsToShuffle
        put state {deck = newDeck, discardPile = []}
        drawFromCurrentDeck cardsToDraw   


-- | Determines which hands won and pays their winnings along with some IO printing.
resolveRound :: StateT GameState IO ()
resolveRound = payHand False
    >> gets split
    >>= \splitPlayed -> when splitPlayed $ payHand True

-- | Resolve hand and pay possible winnings. This also informs the player of the outcome.
payHand :: Bool -- ^ Paying hand 2?
    -> StateT GameState IO ()
payHand splitHand 
    = do
    state <- get
    let playerHand = if splitHand then playerHand2 state else playerHand1 state
        handResult = compareHands playerHand $ dealerHand state
        winning = calculateWin handResult (bet state)
        handNumber = if splitHand then 2 else 1 -- Convert boolean to handnumber int
    liftIO $ putStrLn $ handResultMessage handNumber handResult winning -- Let the player know how the round went
    modify $ payWinnings winning -- Pay the player


-- | Executes the given player action.
--   This doesn't check the legality of the given action.
executeAction :: Action -> StateT GameState IO ()
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
        newState = state {playerHand1 = [newCurrentHand], playerHand2 = newSplitHand, money = newMoney, split = True}
    put newState -- This is not ready yet
    drawForPlayer 1 -- Draw a replacement card to the player's current hand
    -- Temporarily activate the split hand in order to draw a card to it.
    modify $ setHand2Active True 
    drawForPlayer 1
    modify $ setHand2Active False
    liftIO $ putStrLn "Playing the first hand of the split now:"


calculateWin :: HandResult -> Int -> Int 
calculateWin result _ 
    | result == Lose = 0
    | result == BustLose = 0
calculateWin result bet
    | result == Draw = bet -- Bet is returned in case of a draw
    | result == Win || result == DealerBustWin = 2 * bet -- Bet payed back in double in case of normal win
    | result == BlackjackWin = floor $ (2.5 :: Float) * fromIntegral bet-- Blackjack wins are payed at 3:2 odds
    | otherwise = error "Calculating the winning failed"


-- Printing

printChoiceFeedback :: Action -> Maybe Action -> IO ()
printChoiceFeedback _ Nothing = return () -- Nothing to comment
printChoiceFeedback chosenAction (Just rightAction)
    | chosenAction == rightAction   = putStrLn "Correct choice." -- The player had chosen according to theory.
    | otherwise                     = putStrLn $ "You should have chosen: " ++ show rightAction 


-- | A string that informs the player of how the hand ended.
handResultMessage :: Int -- ^ Hand number, 1 or 2
    -> HandResult   -- ^ The result of this hand
    -> Int          -- ^ Amount won. Won't be displayed if the hand didn't win.
    -> String       -- ^ Message to show to the player
handResultMessage handNumber result _ 
    | result == BustLose = handNumberString handNumber ++ "busted, house wins."
    | result == Lose = handNumberString handNumber ++ "lost, house wins."
    | result == Draw = handNumberString handNumber ++ "is a draw, bet is returned."
handResultMessage handNumber result win
    | result == DealerBustWin = "Dealer busted, " ++ handNumberString handNumber ++ winsString win
    | result == Win = handNumberString handNumber ++ winsString win
    | result == BlackjackWin = "Blackjack! " ++ handNumberString handNumber ++ winsString win
    | otherwise = error "Parsing the message of the hand outcome failed"

handNumberString :: Int -> String
handNumberString handNumber = "Hand " ++ show handNumber ++ " "

winsString :: Int -> String
winsString winning = "wins " ++ show winning ++ "!"

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