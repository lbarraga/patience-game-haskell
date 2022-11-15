module Cards where

import Types

import System.Random.Shuffle (shuffle')
import System.Random

-- =================================================
-- ||                  Constanten                 ||
-- =================================================

-- Random seed
seed :: Int
seed = 1

-- | Alle kaarten
allCardsHidden :: [Card]
allCardsHidden = [(cardType, cardValue, Hidden) | cardType <- [Clubs .. Spades], cardValue <- [Ace .. King]]

-- | Alle kaarten zichtbaar (nodig voor render)
allCardsShown :: [Card]
allCardsShown = map showCard allCardsHidden

-- | alle kaarten door elkaar
allShuffledCards :: [Card]
allShuffledCards = shuffleList allCardsHidden

-- de placeholder kaart
placeholderCard :: Card
placeholderCard = (NoneType, NoneValue, NoneStatus)

-- ================================================
-- ||                 Hulp Functies              ||
-- ================================================

-- | Shuffle een lijst.
shuffleList :: [a] -> [a]
shuffleList l = shuffle' l (length l) (mkStdGen seed)

-- c1: de kaart die verplaatst word
-- c2: de kaart waarop c1 geplaatst wordt.
-- region: de `Region` van c2 
-- returns: Of kaart c1 op kaart c2 geplaatst kan worden
canPerformMovement :: Region -> Card -> Card -> Bool
canPerformMovement Pile         c1 c2 = False -- Kan niet op de Pile plaatsen. 
canPerformMovement EndingStacks c1 c2 = (c2 == placeholderCard && isAce  c1) || (getSuit  c1 == getSuit  c2 && c2 `isOneLess` c1)
canPerformMovement GameField    c1 c2 = (c2 == placeholderCard && isKing c1) || (getColor c1 /= getColor c2 && c1 `isOneLess` c2 && isVisible c2)

-- ====================================
-- Triviale getters en kleine functies:
--
-- Ik zou pattern match kunnen gebruikt
-- hebben in bovenstaande functie maar
-- ik vind het zo wat leesbaarder.
-- ===================================


isOneLess :: Card -> Card -> Bool
isOneLess (_, v1, _) (_, v2, _) = fromEnum v1 == fromEnum v2 - 1

showCard :: Card -> Card
showCard (t, v, _) = (t, v, Visible)

isAce :: Card -> Bool
isAce (_, v, _) = v == Ace

isKing :: Card -> Bool
isKing (_, v, _) = v == King

isVisible :: Card -> Bool
isVisible (_, _, status) = status == Visible

getColor :: Card -> CardColor
getColor (Hearts  , _, _) = Red
getColor (Diamonds, _, _) = Red
getColor _ = Black

getSuit :: Card -> CardType
getSuit (v, _, _) = v

getValue :: Card -> Int
getValue (_, value, _) = fromEnum value

