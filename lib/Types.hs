module Types where

-- Een positie op het speelveld.
type Coordinate = (Int, Int)

-- De kleuren van de kaarten.
data CardType = Clubs
              | Diamonds
              | Hearts
              | Spades
              | NoneType
              deriving (Enum, Eq)

instance Show CardType where
    show Clubs    = "club"
    show Diamonds = "diamond"
    show Hearts    = "heart"
    show Spades   = "spade"
    show NoneType = "nonetype"

-- De waarden van de kaarten.
data CardValue = Ace
               | Two
               | Three
               | Four
               | Five
               | Six
               | Seven
               | Eight
               | Nine
               | Ten
               | Jack
               | Queen
               | King
               | NoneValue
               deriving (Enum, Eq)

instance Show CardValue where
    show Ace   = "A"
    show Two   = "2"
    show Three = "3"
    show Four  = "4"
    show Five  = "5"
    show Six   = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine  = "9"
    show Ten   = "10"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"
    show NoneValue = "N"

-- De status van een kaart.
-- Een kaart kan ofwel verborgen zijn, ofwel open liggen.
data CardStatus = Hidden
                | Visible
                | NoneStatus
                deriving (Show, Eq)

-- Een kaart heeft een type, een waarde en een status.
type Card = (CardType, CardValue, CardStatus)

-- Een stapel kaarten.
type Stack = [Card]

-- Representatie van het speelveld.
data Board = Board {
  -- De 7 kolommen met kaarten.
  gameStacks :: [Stack],
  -- De 4 eindstapels.
  endingStacks :: [Stack],
  -- De stapel met kaarten die nog niet op het speelveld liggen.
  pile :: Stack
} deriving (Show)

-- De richting waarin de selector kan bewegen.
data Direction = U | D | L | R deriving (Show)

-- De selector van het spel.
data Selector = Selector {
  -- De huidige positie van de selector.
  position :: Coordinate,
  -- De kaart(en) die de selector momenteel vasthoudt.
  selected :: Maybe Coordinate
} deriving (Show)

-- Het spel.
data Game = Game {
  -- Het speelveld.
  board :: Board,
  -- De selector.
  selector :: Selector
} deriving (Show)


