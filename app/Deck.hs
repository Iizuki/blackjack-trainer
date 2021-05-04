module Deck where

-- Suits are ignored as they are irrelevant in Blackjack.
data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten 
    | Jack | Queen | King | Ace
    deriving (Read, Show, Eq, Enum)


type Deck = [Card]


-- | Returns a normal deck of 52 cards
deck::Deck
deck = [card | card <- [Two .. Ace], _ <- [1 .. 4]]

-- | Returns a deck that consists of n normal decks.
decks:: Int -> Deck
decks n = decks' deck n

decks':: Deck -> Int -> Deck
decks' _ 0 = []
decks' deckSoFar 1 = deckSoFar
decks' deckSoFar n = decks' (deckSoFar ++ deck) (n-1)