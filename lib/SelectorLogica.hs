module SelectorLogica where

import Types
import PatienceLogica (nEndingStacks, nSpeelVeldStapels)
import Cards (canPerformMovement, isVisible)
import BoardLogica (moveSubStack)

import Data.Maybe (isNothing, fromJust)

-- Selecteer een kaart wanneer er nog geen kaart selected was
-- Doe een verplaatsing wanneer er wel al een selected was.
-- De functie `canPlaceSelector` moet altijd voor deze functie opgeroepen worden 
-- om te kijken of de zet wel mogelijk is of niet.
handleSelection :: Maybe Coordinate -> Coordinate -> Game -> Game
handleSelection Nothing            selectorPos = selectCard 
handleSelection (Just selectedPos) selectorPos = deselectCard . moveSubStack selectedPos selectorPos

-- Selecteer de kaart onder de selector
selectCard :: Game -> Game
selectCard g@Game{selector = s@Selector{position = p}} = g{selector = s{selected = Just p}}

-- Deselecteer de huidige selected kaart (doet niets als er geen geselecteerd was).
deselectCard :: Game -> Game
deselectCard g@Game{selector = s} = g{selector = s{selected = Nothing}}

-- Of er vanaf een bepaalde coordinaat in een bepaalde richting gegaan kan worden.
canSelectorMove :: Game -> Coordinate -> Direction -> Bool
canSelectorMove g (EndingStacks, x, _) R = x /= nEndingStacks - 1           -- Kan enkel naar rechts als je je niet op de laatste eindstapel bevind.
canSelectorMove g (EndingStacks, _, _) d = d /= U                           -- Vanaf de eindstapels kan je naar links (kaart of pile) en naar onder (gameveld).
canSelectorMove g (Pile        , _, _) d = d == D || d == R                 -- Vanuit de pile kan je naar onder (gameveld) of de rechts (eindstapels) gaan.
canSelectorMove g (GameField   , x, y) U = True                             -- Je kan altijd naar boven gaan vanaf speelveld (hogere kaart, pile of endingstapels).
canSelectorMove g (GameField   , x, y) d = isInGameField (x + dx, y + dy) g -- True als de bestemming binnen het speelveld ligt.
    where (dx, dy) = dirToDelta d

-- Of een bepaalde coordinaat zich in het gameVeld bevind (Dit gaat dus niet over de pile of endingstacks)
isInGameField :: (Int, Int) -> Game -> Bool
isInGameField (x, y) game = containsHor && containsVer
    where containsHor = x >= 0 && x < nSpeelVeldStapels
          containsVer = y >= 0 && y < length stack
          stack = ((!! x) . gameStacks  . board) game

-- Of de selector geplaatst kan worden. Er zijn twee mogelijkheden:
-- 1. Er is nog geen kaart geselecteerd -> kan de huidge kaart geselecteerd worden?
-- 2. Er is al een kaart geselecteerd   -> kan de verplaatsing gebeurden?
canPlaceSelector :: (Card, Card) -> Coordinate -> Maybe Coordinate -> Bool
canPlaceSelector (_   , onto) selectorPos    Nothing   = canSelect onto
canPlaceSelector (from, onto) (region, _, _) (Just _)  = canPerformMovement region from onto

-- Of een enkele kaart geselecteerd kan worden met de selector.
-- Dit wordt enkel opgeroepen wanneer er nog geen selected card is 
canSelect :: Card -> Bool
canSelect = isVisible

-- Geeft een potentiele beweging (`from`, `onto`).
-- De eerste kaart in de tupel: `from`, stelt de onderste kaart voor in de substapel die weggenomen wordt
-- De tweede kaart in de tupel: `onto`, stelt de kaart voor waarop die substapel gelegd zal worden
getPotentialMovement :: Game -> (Card, Card)
getPotentialMovement g = (getCardFromCo (fromJust from) g, getLastFromCo onto g)
    where from = (selected . selector) g
          onto = (position . selector) g
          
-- Geef de kaart gelegen op een bepaalde coordinaat
getCardFromCo :: Coordinate -> Game -> Card
getCardFromCo (Pile,         _, _) = last   . pile   . board
getCardFromCo (EndingStacks, x, _) = last   . (!! x) . endingStacks . board
getCardFromCo (GameField,    x, y) = (!! y) . (!! x) . gameStacks   . board

-- Geef de bovenste kaart van de stapel waar een coordinaat zich bevind.
getLastFromCo :: Coordinate -> Game -> Card
getLastFromCo (GameField, x, _) = last . (!! x) . gameStacks . board
getLastFromCo co = getCardFromCo co



