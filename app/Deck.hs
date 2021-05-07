module Deck (Deck, Card, shuffledDecks, draw) where

import System.Random (newStdGen, RandomGen (genRange))
import System.Random.Shuffle (shuffle')

-- Suits are ignored as they are irrelevant in Blackjack.
data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten 
    | Jack | Queen | King | Ace
    deriving (Read, Show, Eq, Enum)


type Deck = [Card]


-- | Returns a normal deck of 52 cards
deck::Deck
deck = [card | card <- [Two .. Ace], _ <- [1 .. 4]]

-- | Returns a deck that consists of n normal decks.
decks :: Int -> Deck
decks n = decks' deck n

decks' :: Deck -> Int -> Deck
decks' _ 0 = []
decks' deckSoFar 1 = deckSoFar
decks' deckSoFar n = decks' (deckSoFar ++ deck) (n-1)


-- | Returns a deck that consists of n decks shuffled together.
shuffledDecks :: Int -> IO Deck
shuffledDecks numberOfDecks = do
    let orderedDecks = decks numberOfDecks
    rng <- newStdGen
    let shuffledDecks = shuffle' orderedDecks (length orderedDecks) rng :: Deck
    return shuffledDecks

-- | Draw a number of cards from deck.
draw :: Deck -> Int -> (Deck, [Card])
draw deckToDrawFrom numberOfCards = splitAt numberOfCards deckToDrawFrom
