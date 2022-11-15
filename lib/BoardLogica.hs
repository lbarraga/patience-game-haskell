module BoardLogica where

import Types
import PatienceLogica
import Cards (placeholderCard, showCard, allCardsHidden, allShuffledCards)

import Data.Maybe (fromJust)


-- =====================================================
-- ||                   Constanten                    ||
-- =====================================================

-- Aantal ending stacks
nEndingStacks :: Int
nEndingStacks = 4

-- Het aantal oplopende stapels op het speelveld
nSpeelVeldStapels :: Int
nSpeelVeldStapels = 7

-- Het aantal kaarten op het speelveld
aantalSpeelVeldKaarten :: Int
aantalSpeelVeldKaarten = n*(n+1) `div` 2 -- som van eerste n getallen 
    where n = nSpeelVeldStapels

-- De initiele kaarten op het SpeelVeld
initGameField :: [Stack]
initGameField = makeGameField (take aantalSpeelVeldKaarten allShuffledCards) nSpeelVeldStapels

-- De initiele kaarten op de pile (kaarten die overgehouden zijn na het nemen van de speelVeldKaarten)
initPile :: Stack
initPile = map showCard (drop aantalSpeelVeldKaarten allShuffledCards)

-- de initiele kaarten op de endingstacks
initEndingStacks :: [Stack]
initEndingStacks = replicate nEndingStacks [placeholderCard]

-- Initiele opstelling van het bord
initBoard :: Board
initBoard = Board {
    gameStacks = initGameField,
    pile = initPile,
    endingStacks = initEndingStacks
}


-- =====================================================
-- ||                  Hulp Functies                  ||
-- =====================================================

-- Maak stapels van oplopende grootte: van 1, 2, ..., tot grootte maxPile
makeGameField :: [Card] -> Int -> [Stack]
makeGameField cards 0 = []
makeGameField cards maxPile = previousStacks ++ [showLast stack]
    where stack = take maxPile cards
          previousStacks = makeGameField (drop maxPile cards) (maxPile - 1)

-- show last card in a stack
showLast :: Stack -> Stack
showLast [] = [placeholderCard]
showLast stack = init stack ++ [showCard $ last stack]


-- Of er vanaf een bepaalde coordinaat in een bepaalde richting gegaan kan worden. 
canMoveInDirection :: Coordinate -> Board -> Direction -> Bool
canMoveInDirection (EndingStacks, x, _) _ R  = x /= nEndingStacks - 1            -- Kan enkel naar rechts als je je niet op de laatste eindstapel bevind.
canMoveInDirection (EndingStacks, _, _) _ d  = d /= U                            -- Vanaf de eindstapels kan je naar links (kaart of pile) en naar onder (gameveld).
canMoveInDirection (Pile        , _, _) _ d  = d == D || d == R                  -- Vanuit de pile kan je naar onder (gameveld) of de rechts (eindstapels) gaan.
canMoveInDirection (GameField   , x, y) _ U  = True                              -- Je kan altijd naar boven gaan vanaf speelveld (hogere kaart, pile of endingstapels).
canMoveInDirection (GameField   , x, y) b d  = isInGameField (x + dx, y + dy) gs -- True als de bestemming binnen het speelveld ligt.
    where (dx, dy) = dirToDelta d
          gs = gameStacks b

moveInDirection :: Coordinate -> Direction -> Coordinate
moveInDirection (Pile        , _, _) R  = (EndingStacks, 0, 0)
moveInDirection (Pile        , _, _) D  = (GameField   , 0, 0)
moveInDirection (EndingStacks, 0, _) L  = (Pile        , 0, 0)
moveInDirection (EndingStacks, x, _) D  = (GameField   , x + 3, 0)
moveInDirection (GameField   , x, 0) U
  | x >= 2                              = (EndingStacks, max (x - 3) 0, 0)
  | otherwise                           = (Pile, 0, 0)
moveInDirection (region, x, y) dir      = (region, x + dx, y + dy)
    where (dx, dy) = dirToDelta dir

-- Geeft een potentiele beweging (`from`, `onto`).
-- De eerste kaart in de tupel: `from`, stelt de onderste kaart voor in de substapel die weggenomen wordt
-- De tweede kaart in de tupel: `onto`, stelt de kaart voor waarop die substapel gelegd zal worden
getPotentialMovement :: Coordinate -> Coordinate -> Board -> (Card, Card)
getPotentialMovement from onto@(EndingStacks, _, _) b = (getLastFromCo from b, getCardFromCo onto b)
getPotentialMovement from onto@(GameField   , _, _) b = (getCardFromCo from b, getLastFromCo onto b)
getPotentialMovement from onto@(Pile        , _, _) b = error "Internal error: kan niet op Pile plaatsen."


-- Of een bepaalde coordinaat zich in het gameVeld bevind (Dit gaat dus niet over de pile of endingstacks)
isInGameField :: (Int, Int) -> [Stack] -> Bool
isInGameField (x, y) gameField = containsHor && containsVer
    where containsHor = x >= 0 && x < nSpeelVeldStapels
          containsVer = y >= 0 && y < length stack
          stack = gameField !! x

moveSubStack :: Coordinate -> Coordinate -> Board -> Board
moveSubStack from to b = boardAfterPlacingStack to removed removedBoard
    where (removed, removedBoard) = takeStackAction fromCorrected b
          fromCorrected = correction from to b 

correction :: Coordinate -> Coordinate -> Board -> Coordinate
correction (GameField, x, y) (EndingStacks, _, _) b = (GameField, x, (length . (!! x) . gameStacks) b - 1)
correction from _ _ = from

-- Verwijder een substapel kaarten van het bord,
-- geeft de substapel terug en het bord na de verwijder actie
takeStackAction :: Coordinate -> Board -> (Stack, Board)
takeStackAction co b = (takeStack co b, boardAfterRemove co b)


takeStack :: Coordinate -> Board -> Stack
takeStack (Pile, _, _)         b = [last $ pile b]
takeStack (EndingStacks, x, _) b = [last $ endingStacks b !! x]
takeStack (GameField, x, y)    b = drop y (gameStacks b !! x)

boardAfterRemove :: Coordinate -> Board -> Board
boardAfterRemove co@(Pile        , _, _) b = b{pile         = stackAfterRemove co b}
boardAfterRemove co@(EndingStacks, x, _) b = b{endingStacks = replaceStack x (endingStacks b) (stackAfterRemove co b)}
boardAfterRemove co@(GameField   , x, _) b = b{gameStacks   = replaceStack x (gameStacks   b) (stackAfterRemove co b)}

stackAfterRemove :: Coordinate -> Board -> Stack
stackAfterRemove (Pile        , _, _) = showLast . init   . pile
stackAfterRemove (EndingStacks, x, _) = showLast . init   . (!! x) . endingStacks
stackAfterRemove (GameField   , x, y) = showLast . take y . (!! x) . gameStacks

checkPlaceholder :: Stack -> Stack
checkPlaceholder (x:xs)
  | x == placeholderCard = xs
  | otherwise            = x:xs


placeOnStack :: [Stack] -> Int -> Stack -> [Stack]
placeOnStack stacks replaceIndex stack = replaceStack replaceIndex stacks newEndingStack
    where newEndingStack = checkPlaceholder (stacks !! replaceIndex ++ stack)


boardAfterPlacingStack :: Coordinate ->  Stack -> Board -> Board
boardAfterPlacingStack (EndingStacks, x, _) s b = b{endingStacks = placeOnStack (endingStacks b) x s}
boardAfterPlacingStack (GameField   , x, _) s b = b{gameStacks   = placeOnStack (gameStacks   b) x s}

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

