module GameLogica where

import Types

import SelectorLogica (handleSelection, canSelectorMove, canPlaceSelector, getPotentialMovement)
import PatienceLogica (moveSelectorPos, rotateStackNTimes)

import Data.Maybe (fromJust, isNothing)
   
-- Tussenfunctie voor properheid
handleGameSelection :: Game -> Game
handleGameSelection g = handleSelection maybeSelected selectorPos g
      where maybeSelected = (selected . selector) g
            selectorPos   = (position . selector) g
            
-- Tussenfunctie voor properheid 
canGameSelectorMove :: Game -> Direction -> Bool
canGameSelectorMove g@Game{selector = s@Selector{position = p}} = canSelectorMove g p 

-- TussenFuncie evoor properheid
canGamePlaceSelector :: Game -> Bool
canGamePlaceSelector g@Game{selector = Selector{position = p, selected = s}} = canPlaceSelector (from, onto) p s
    where (from, onto) = getPotentialMovement g

-- TussenFunctie voor properheid


-- move de selector van de game
move :: Game -> Direction -> Game
move g@Game{selector = s} dir = g{selector = moveSelector s dir}



-- Move de selector 
moveSelector :: Selector -> Direction -> Selector
moveSelector s@Selector{position = p} dir = s{position = moveSelectorPos p dir}


rotatePile :: Game -> Game
rotatePile g@Game{board = b@Board{pile = p}} = g{ board = b{pile = newPile}}
    where newPile = rotateStackNTimes p 2














