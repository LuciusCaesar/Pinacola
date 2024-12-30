-- | A module for the card game Pinacola.
module Pinacola where

import Control.Monad.Random

-- * Data types

-- | A game of Pinacola.
data PinacolaGame = PinacolaGame { 
    gPlayers :: [Player]
    , gDeck :: Deck
    , gDefausse :: Deck
    }
    deriving (Eq, Show)

data PlayedCards = Pocker [Card] | Pinacola [Card]

-- | A player.
data Player = Player { 
    pName :: String
    , pHand :: [Card] 
    , pTeam :: Team
    }
    deriving (Eq, Show)

-- | A team.
data Team = Team1 | Team2
    deriving (Eq, Show)

-- | A deck is a list of cards.  
type Deck = [Card]

-- | A card.
data Card = Card Suit Value
          | NoCard
    deriving (Eq, Ord, Show)

-- | The suit of a card.
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | The value of a card.
data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Eq, Ord, Enum, Bounded, Show)

-- * Initialization functions

-- | A standard deck of 52 cards.
simpleDeck :: Deck
simpleDeck = [Card s v | s <- [Clubs .. Spades], v <- [Ace .. King]]

-- | A double deck of 104 cards.
doubleDeck :: Deck
doubleDeck = simpleDeck <> simpleDeck

-- | A triple deck of 156 cards.
tripleDeck :: Deck
tripleDeck = simpleDeck <> simpleDeck <> simpleDeck

-- | Shuffle a list of elements.
shuffle :: Deck -> Rand StdGen Deck
shuffle xs = do
    (xs', _) <- distribute (length xs) xs []
    return xs'

-- | Pick a random card from a deck.
pickRandomCard :: Deck -> Rand StdGen (Card, Deck)
pickRandomCard [] = return (NoCard, [])
pickRandomCard [c] = return (c, [])
pickRandomCard cs = do
    i <- getRandomR (0, length cs - 1)
    let (left, right) = splitAt i cs
    return (head right, left ++ tail right)

-- | Pick n random cards from the deck to create a list of cards and the remaining deck.
distribute 
  :: Int -- ^ The number of cards to pick
  -> Deck -- ^ The deck from which to pick
  -> Deck -- ^ The accumulator to which to add the picked cards
  -> Rand StdGen (Deck, Deck) -- ^ Rand StdGen (picked cards, remaining deck)
distribute 0 deck acc = return (acc, deck)
distribute _ [] acc = return (acc, [])
distribute n deck acc = do
      (card, deck') <- pickRandomCard deck
      distribute (n - 1) deck' (card : acc)

-- | Setup a game of Pinacola.
setupGame :: Rand StdGen PinacolaGame
setupGame = 
  let 
    n = 13
  in do
    deck <- shuffle doubleDeck -- Shuffling the deck is not necessary as distributing cards is random.  
    (hand1, deck') <- distribute n deck []
    (hand2, deck'') <- distribute n deck' []
    (scartata, deck''') <- distribute 1 deck'' []
    let player1 = Player "Alice" hand1 Team1
    let player2 = Player "Bob" hand2 Team2
    return $ PinacolaGame [player1, player2] deck''' scartata

play :: IO ()
play = do
  game <- evalRandIO setupGame
  print game

-- * Helper functions

-- | Make a list even by removing the last element if the length is odd.
makeEven :: [a] -> [a]
makeEven [] = []
makeEven [_] = []
makeEven xs@(_ : ps) = if even (length xs) then xs else ps