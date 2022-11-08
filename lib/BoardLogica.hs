module BoardLogica where

import Types
import PatienceLogica
import Cards (placeholderCard)

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
