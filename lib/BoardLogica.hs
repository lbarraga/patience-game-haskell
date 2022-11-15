module BoardLogica where

import Types
import PatienceLogica
import Cards (placeholderCard)

import Data.Maybe (fromJust)

-- Of er vanaf een bepaalde coordinaat in een bepaalde richting gegaan kan worden. 
canMoveInDirection :: Coordinate -> Board -> Direction -> Bool
canMoveInDirection (EndingStacks, x, _) _ R  = x /= nEndingStacks - 1            -- Kan enkel naar rechts als je je niet op de laatste eindstapel bevind.
canMoveInDirection (EndingStacks, _, _) _ d  = d /= U                            -- Vanaf de eindstapels kan je naar links (kaart of pile) en naar onder (gameveld).
canMoveInDirection (Pile        , _, _) _ d  = d == D || d == R                  -- Vanuit de pile kan je naar onder (gameveld) of de rechts (eindstapels) gaan.
canMoveInDirection (GameField   , x, y) _ U  = True                              -- Je kan altijd naar boven gaan vanaf speelveld (hogere kaart, pile of endingstapels).
canMoveInDirection (GameField   , x, y) b d  = isInGameField (x + dx, y + dy) gs -- True als de bestemming binnen het speelveld ligt.
    where (dx, dy) = dirToDelta d
          gs = gameStacks b

-- Of een bepaalde coordinaat zich in het gameVeld bevind (Dit gaat dus niet over de pile of endingstacks)
isInGameField :: (Int, Int) -> [Stack] -> Bool
isInGameField (x, y) gameField = containsHor && containsVer
    where containsHor = x >= 0 && x < nSpeelVeldStapels
          containsVer = y >= 0 && y < length stack
          stack = gameField !! x

moveSubStack :: Coordinate -> Coordinate -> Game -> Game
moveSubStack from to g@Game{board = b} = g{board = newBoard}
    where newBoard = boardAfterPlacingStack to removed removedBoard
          (removed, removedBoard) = takeStackAction from b 
        
          
-- Verwijder een substapel kaarten van het bord,
-- geeft de substapel terug en het bord na de verwijder actie
takeStackAction :: Coordinate -> Board -> (Stack, Board)
takeStackAction co b = (takeStack co b, boardAfterRemove co b)


takeStack :: Coordinate -> Board -> Stack
takeStack (Pile, _, _)         b = [last $ pile b]
takeStack (EndingStacks, x, _) b = [last $ endingStacks b !! x]
takeStack (GameField, x, y)    b = drop y (gameStacks b !! x)


boardAfterRemove :: Coordinate -> Board -> Board
boardAfterRemove (Pile, _, _)         b = b{pile = (showLast . init) $ pile b}
boardAfterRemove (EndingStacks, x, _) b = b{endingStacks = replaceStack x stacks newEndingStack}
    where newEndingStack = (showLast . init) (stacks !! x) -- laatste is weg
          stacks = endingStacks b
boardAfterRemove (GameField, x, y)    b = b{gameStacks = replaceStack x stacks newGameStack}
    where newGameStack = (showLast . take y) (stacks !! x) -- alle kaarten vanaf co y zijn weg 
          stacks = gameStacks b


checkPlaceholder :: Stack -> Stack
checkPlaceholder (x:xs)
  | x == placeholderCard = xs
  | otherwise            = x:xs


placeOnStack :: [Stack] -> Int -> Stack -> [Stack]
placeOnStack stacks replaceIndex stack = replaceStack replaceIndex stacks newEndingStack
    where newEndingStack = checkPlaceholder (stacks !! replaceIndex ++ stack)


boardAfterPlacingStack :: Coordinate ->  Stack -> Board -> Board
boardAfterPlacingStack (EndingStacks, x, _) s b = b{endingStacks = placeOnStack (endingStacks b) x s}
boardAfterPlacingStack (GameField, x, _)    s b = b{gameStacks   = placeOnStack (gameStacks   b) x s}


replaceStack :: Int -> [Stack] -> Stack -> [Stack]
replaceStack i stacks with = before ++ [with] ++ after
    where (before, _:after) = splitAt i stacks
          
-- Geef de kaart gelegen op een bepaalde coordinaat
getCardFromCo :: Coordinate -> Board -> Card
getCardFromCo (Pile,         _, _) = last   . pile
getCardFromCo (EndingStacks, x, _) = last   . (!! x) . endingStacks
getCardFromCo (GameField,    x, y) = (!! y) . (!! x) . gameStacks

-- Geef de bovenste kaart van de stapel waar een coordinaat zich bevind.
getLastFromCo :: Coordinate -> Board -> Card
getLastFromCo (GameField, x, _) = last . (!! x) . gameStacks
getLastFromCo co = getCardFromCo co

