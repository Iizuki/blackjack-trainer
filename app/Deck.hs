module Deck (Deck, Card(..), shuffledDecks, draw) where

import System.Random (newStdGen, RandomGen (genRange))
import System.Random.Shuffle (shuffle')

-- Suits are ignored as they are irrelevant in Blackjack.
data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten 
    | Jack | Queen | King | Ace
    deriving (Read, Eq)


type Deck = [Card]

instance Show Card where
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"

instance Enum Card where
    fromEnum Ace     = 1 -- Ace being 11 will be dealt with when summing the cards
    fromEnum Two     = 2
    fromEnum Three   = 3
    fromEnum Four    = 4
    fromEnum Five    = 5
    fromEnum Six     = 6
    fromEnum Seven   = 7
    fromEnum Eight   = 8
    fromEnum Nine    = 9
    fromEnum Ten     = 10
    fromEnum Jack    = 10
    fromEnum Queen   = 10
    fromEnum King    = 10

    toEnum _ = King -- No need to go this way

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
draw :: Deck -> Int -> ([Card], Deck)
draw deckToDrawFrom numberOfCards = splitAt numberOfCards deckToDrawFrom
