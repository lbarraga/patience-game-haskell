module GameLogica where

import Types
import Cards (canPerformMovement, isVisible)
import PatienceLogica (rotateStackNTimes, moveSelectorPos, nEndingStacks, nSpeelVeldStapels)
import BoardLogica (moveSubStack)

import Data.Maybe (fromJust, isNothing)

-- Of de selector geplaatst kan worden. Er zijn twee mogelijkheden:
-- 1. Er is nog geen kaart geselecteerd -> kan de huidge kaart geselecteerd worden?
-- 2. Er is al een kaart geselecteerd   -> kan de verplaatsing gebeurden?
canPlaceSelector :: Game -> Bool
canPlaceSelector g
  | isNothing selectedPos = canSelect selectorPos g
  | otherwise             = canPerformMovement region card onto
  where selectorPos@(region, _, _) = (position . selector) g
        selectedPos                = (selected . selector) g
        (card, onto) = getPotentialMovement g
        
-- move de selector van de game
move :: Game -> Direction -> Game
move g@Game{selector = s} dir = g{selector = moveSelector s dir}

-- Of een kaart geselecteerd kan worden of niet. 
canSelect :: Coordinate -> Game -> Bool
canSelect co = isVisible . getCardFromCo co

-- Move de selector 
moveSelector :: Selector -> Direction -> Selector
moveSelector s@Selector{position = p} dir = s{position = moveSelectorPos p dir}


rotatePile :: Game -> Game
rotatePile g@Game{board = b@Board{pile = p}} = g{ board = b{pile = newPile}}
    where newPile = rotateStackNTimes p 2


getPotentialMovement :: Game -> (Card, Card)
getPotentialMovement g = (getCardFromCo (fromJust from) g, getLastFromCo onto g)
    where from = (selected . selector) g
          onto = (position . selector) g


-- Geef de bovenste kaart van de stapel waar een coordinaat zich bevind.
getLastFromCo :: Coordinate -> Game -> Card
getLastFromCo (GameField, x, y) = last . (!! x) . gameStacks . board
getLastFromCo co = getCardFromCo co


-- Ga van een coordinaat naar een kaart.
getCardFromCo :: Coordinate -> Game -> Card
getCardFromCo (Pile,         _, _) = last   . pile   . board
getCardFromCo (EndingStacks, x, _) = last   . (!! x) . endingStacks . board
getCardFromCo (GameField,    x, y) = (!! y) . (!! x) . gameStacks   . board


handleGameSelection :: Game -> Game
handleGameSelection g = handleSelection maybeSelected selectorPos g
      where maybeSelected = (selected . selector) g
            selectorPos   = (position . selector) g


handleSelection :: Maybe Coordinate -> Coordinate -> Game -> Game
handleSelection Nothing            selectorPos = selectCard 
handleSelection (Just selectedPos) selectorPos = deselect . moveSubStack selectedPos selectorPos


deselect :: Game -> Game
deselect g@Game{selector = s} = g{selector = s{selected = Nothing}}


selectCard :: Game -> Game
selectCard g@Game{selector = s@Selector{position = p}} = g{selector = s{selected = Just p}}

canMove :: Game -> Coordinate -> Direction -> Bool
canMove g (GameField, x, y) dir@(dx, dy)   
  | dir == up = True                                  -- Je kan altijd naar boven gaan vanaf speelveld (hogere kaart, pile of endingstapels).
  | otherwise = isInGameField (x + dx, y + dy) g      -- True als de bestemming binnen het speelveld ligt.
canMove g (EndingStacks, x, _) dir
  | dir == right = x /= nEndingStacks - 1             -- Kan enkel naar rechts als je je niet op de laatste eindstapel bevind.
  | otherwise    = dir `elem` [left, down]            -- Vanaf de eindstapels kan je naar links (kaart of pile) en naar onder (gameveld).
canMove g (Pile, _, _) dir = dir `elem` [right, down] -- Vanuit de pile kan je naar onder (gameveld) of de rechts (eindstapels) gaan.

isInGameField :: (Int, Int) -> Game -> Bool
isInGameField (x, y) game = x `elem` [0..(nSpeelVeldStapels - 1)] && y `elem` [0..(length (stacks !! x) - 1)]
    where stacks = (gameStacks . board) game

canSelectorMove :: Game -> Direction -> Bool
canSelectorMove g@Game{selector = s@Selector{position = p}} = canMove g p
