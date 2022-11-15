module GameLogica where

import Types

import SelectorLogica
import BoardLogica (initBoard, canMoveInDirection, moveSubStack, getCardFromCo, getLastFromCo, moveInDirection, getPotentialMovement)
import Cards (canPerformMovement)

import Data.Maybe (fromJust, isNothing)

-- =================================================================
-- ||                       Constanten                            ||
-- =================================================================

-- Initiele opstelling van de game
initGame :: Game
initGame = Game {board = initBoard, selector = initSelector}

-- =================================================================
-- ||                     Tussenfuncties                          ||
-- =================================================================
   
-- Tussenfunctie voor properheid:
-- Roept handleSelection op met de selected-positie en de selector-positie
handleGameSelection :: Game -> Game
handleGameSelection g = placeSelector maybeSelected selectorPos g 
      where (maybeSelected, selectorPos) = (getBothSelections . selector) g

-- TussenFunctie evoor properheid
canGamePlaceSelector :: Game -> Bool
canGamePlaceSelector g@Game{selector = Selector{position = p, selected = s}} = canPlaceSelector p s g

-- Tussenfunctie voor properheid
-- roept `canPerformMovement` op vanuit de bordklasse. Geeft de regio mee en de potentielemovement 
canGamePerformMovement :: Coordinate -> Game -> Bool
canGamePerformMovement (region, _, _) = uncurry (canPerformMovement region) . getPotentialMovementGame

-- Tussenfunctie voor properheid
canGameSelect :: Game -> Bool
canGameSelect = canSelect . getSelectedCard

-- TussenFunctie voor properheid: vraagt aan selectormodule om te selecteren.
gameSelect :: Game -> Game
gameSelect g@Game{selector = s} = g{selector = select s}

-- TussenFunctie voor properheid: vraagt aan selectormodule om te deselecteren.
gameDeselect :: Game -> Game
gameDeselect g@Game{selector = s} = g{selector = deselect s}

-- tussenFunctie voor properheid
moveGameSubstack :: Coordinate -> Coordinate -> Game -> Game
moveGameSubstack selectedPos selectorPos g@Game{board = b} = g{board = moveSubStack selectedPos selectorPos b}

-- tussenfunctie voor properheid
getPotentialMovementGame :: Game -> (Card, Card) 
getPotentialMovementGame g = getPotentialMovement (fromJust selectedCo) selectorCo (board g)
      where (selectedCo, selectorCo) = (getBothSelections . selector) g

-- tussenfunctie voor properheid
move :: Game -> Direction -> Game
move g@Game{selector = s} dir = g{selector = moveSelector s dir}

-- =================================================================
-- ||                     Volledige gamefuncties                  ||
-- =================================================================

-- Vraagt aan de boardmodule of er vanuit de positie van de selector in een bepaalde richting gegaan kan worden.
canGameSelectorMove :: Game -> Direction -> Bool
canGameSelectorMove g = canMoveInDirection selectorPos (board g)
      where selectorPos = (position . selector) g

-- Of de selector geplaatst kan worden. Er zijn twee mogelijkheden:
-- 1. Er is nog geen kaart geselecteerd -> kan de huidge kaart geselecteerd worden?
-- 2. Er is al een kaart geselecteerd   -> kan de verplaatsing gebeurden?
canPlaceSelector :: Coordinate -> Maybe Coordinate -> Game -> Bool
canPlaceSelector selectorPos Nothing  = canGameSelect
canPlaceSelector selectorPos (Just _) = canGamePerformMovement selectorPos

-- Selecteer een kaart wanneer er nog geen kaart selected was
-- Doe een verplaatsing wanneer er wel al een selected was.
-- De functie `canPlaceSelector` moet altijd voor deze functie opgeroepen worden 
-- om te kijken of de zet wel mogelijk is of niet.
placeSelector :: Maybe Coordinate -> Coordinate -> Game -> Game
placeSelector Nothing            selectorPos = gameSelect
placeSelector (Just selectedPos) selectorPos = gameDeselect . moveGameSubstack selectedPos selectorPos

-- Geef de kaart momenteel onder de selector
getSelectedCard :: Game -> Card
getSelectedCard g = getCardFromCo co (board g)
      where co = (position . selector) g

-- Move de selector 
moveSelector :: Selector -> Direction -> Selector
moveSelector s@Selector{position = p} dir = s{position = moveInDirection p dir}

-- Roteer de pile met 3
rotatePile :: Game -> Game
rotatePile g@Game{board = b@Board{pile = p}} = g{ board = b{pile = newPile}}
    where newPile = rotateStackNTimes p 2

-- roteer een lijst van kaarten
rotateStackNTimes :: Stack -> Int -> Stack
rotateStackNTimes [] _     = []
rotateStackNTimes l 0      = l
rotateStackNTimes (x:xs) n = rotateStackNTimes (xs ++ [x]) (n - 1)
