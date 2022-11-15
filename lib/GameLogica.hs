module GameLogica where

import Types

import SelectorLogica
import PatienceLogica (moveSelectorPos, rotateStackNTimes)
import BoardLogica (canMoveInDirection, moveSubStack, getCardFromCo, getLastFromCo)
import Cards (canPerformMovement)

import Data.Maybe (fromJust, isNothing)
   
-- Tussenfunctie voor properheid
handleGameSelection :: Game -> Game
handleGameSelection g = handleSelection maybeSelected selectorPos g 
      where (maybeSelected, selectorPos) = (getBothSelections . selector) g

-- Selecteer een kaart wanneer er nog geen kaart selected was
-- Doe een verplaatsing wanneer er wel al een selected was.
-- De functie `canPlaceSelector` moet altijd voor deze functie opgeroepen worden 
-- om te kijken of de zet wel mogelijk is of niet.
handleSelection :: Maybe Coordinate -> Coordinate -> Game -> Game
handleSelection Nothing            selectorPos = gameSelect 
handleSelection (Just selectedPos) selectorPos = gameDeselect . moveSubStack selectedPos selectorPos
            
-- Tussenfunctie voor properheid 
canGameSelectorMove :: Game -> Direction -> Bool
canGameSelectorMove g@Game{selector = s@Selector{position = selectorPos}} = canMoveInDirection selectorPos (board g)

-- TussenFunctie evoor properheid
canGamePlaceSelector :: Game -> Bool
canGamePlaceSelector g@Game{selector = Selector{position = p, selected = s}} = canPlaceSelector g p s

-- Of de selector geplaatst kan worden. Er zijn twee mogelijkheden:
-- 1. Er is nog geen kaart geselecteerd -> kan de huidge kaart geselecteerd worden?
-- 2. Er is al een kaart geselecteerd   -> kan de verplaatsing gebeurden?
canPlaceSelector :: Game -> Coordinate -> Maybe Coordinate -> Bool
canPlaceSelector g selectorPos    Nothing  = canGameSelect g
canPlaceSelector g (region, _, _) (Just _) = canPerformMovement region from onto
    where (from, onto) = getPotentialMovement g

-- Geeft een potentiele beweging (`from`, `onto`).
-- De eerste kaart in de tupel: `from`, stelt de onderste kaart voor in de substapel die weggenomen wordt
-- De tweede kaart in de tupel: `onto`, stelt de kaart voor waarop die substapel gelegd zal worden
getPotentialMovement :: Game -> (Card, Card)
getPotentialMovement g = (getCardFromCo (fromJust from) b, getLastFromCo onto b)
    where (from, onto) = (getBothSelections . selector) g
          b    = board g
-- Tussenfunctie voor properheid
canGameSelect :: Game -> Bool
canGameSelect = canSelect . getSelectedCard

getSelectedCard :: Game -> Card
getSelectedCard g = getCardFromCo co (board g)
      where co = (position . selector) g

-- TussenFunctie voor properheid
gameSelect :: Game -> Game
gameSelect g@Game{selector = s} = g{selector = select s}

-- TussenFunctie voor properheid
gameDeselect :: Game -> Game
gameDeselect g@Game{selector = s} = g{selector = deselect s}

-- move de selector van de game
move :: Game -> Direction -> Game
move g@Game{selector = s} dir = g{selector = moveSelector s dir}



-- Move de selector 
moveSelector :: Selector -> Direction -> Selector
moveSelector s@Selector{position = p} dir = s{position = moveSelectorPos p dir}


rotatePile :: Game -> Game
rotatePile g@Game{board = b@Board{pile = p}} = g{ board = b{pile = newPile}}
    where newPile = rotateStackNTimes p 2














