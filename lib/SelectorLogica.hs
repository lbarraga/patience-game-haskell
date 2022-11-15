module SelectorLogica where

import Types
import PatienceLogica (nEndingStacks, nSpeelVeldStapels)
import Cards (canPerformMovement, isVisible)


import Data.Maybe (isNothing, fromJust)

-- Selecteer de kaart onder de selector
select :: Selector -> Selector
select s@Selector{position = selectorPosition} = s{selected = Just selectorPosition}

-- Deselecteer de huidige selected kaart (doet niets als er geen geselecteerd was).
deselect :: Selector -> Selector
deselect selector = selector{selected = Nothing}

-- Of een enkele kaart geselecteerd kan worden met de selector.
-- Dit wordt enkel opgeroepen wanneer er nog geen selected card is 
canSelect :: Card -> Bool
canSelect = isVisible

getBothSelections :: Selector -> (Maybe Coordinate, Coordinate)
getBothSelections Selector{position = selectorPos, selected = selectedPos} = (selectedPos, selectorPos)