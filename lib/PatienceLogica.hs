module PatienceLogica where

import Types

-- === Constanten == 

-- | Alle kaarten
allCards :: [Card]
allCards = [(cardType, cardValue, Hidden) | cardType <- [Clubs .. Spades], cardValue <- [Ace .. King]]

-- Aantal ending stacks
nEndingStacks :: Int
nEndingStacks = 4

-- de placeholder kaart
placeholderCard :: Card
placeholderCard = (NoneType, NoneValue, NoneStatus)

-- Het aantal oplopende stapels op het speelveld
nSpeelVeldStapels :: Int
nSpeelVeldStapels = 7

-- Het aantal kaarten op het speelveld
aantalSpeelVeldKaarten :: Int
aantalSpeelVeldKaarten = n*(n+1) `div` 2 -- som van eerste n getallen 
    where n = nSpeelVeldStapels

-- De initiele kaarten op het SpeelVeld
initGameField :: [Stack]
initGameField = makeGameField (take aantalSpeelVeldKaarten allCards) nSpeelVeldStapels

-- De initiele kaarten op de pile (kaarten die overgehouden zijn na het nemen van de speelVeldKaarten)
initPile :: Stack
initPile = map showCard (drop aantalSpeelVeldKaarten allCards)

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

-- Initiele staat van de selector.
initSelector :: Selector
initSelector = Selector {position = (Pile, 0, 0), selected = Nothing}

-- Initiele opstelling van de game
initGame :: Game
initGame = Game {board = initBoard, selector = initSelector}

-- === HulpFuncties ===

-- Maak stapels van oplopende grootte: van 1, 2, ..., tot grootte maxPile
makeGameField :: [Card] -> Int -> [Stack]
makeGameField cards 0 = []
makeGameField cards maxPile = previousStacks ++ [showLast stack]
    where stack = take maxPile cards
          previousStacks = makeGameField (drop maxPile cards) (maxPile - 1)

showCard :: Card -> Card
showCard (t, v, _) = (t, v, Visible)

-- show last card in a stack
showLast :: Stack -> Stack
showLast [] = [placeholderCard]
showLast stack = init stack ++ [showCard $ last stack]

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

moveSelectorPos :: Coordinate -> Direction -> Coordinate
moveSelectorPos (GameField, x, 0) (0, -1)  -- UP TODO iets bedenken
  | x >= 2    = (EndingStacks, max (x - 3) 0 , 0)
  | otherwise = (Pile, 0, 0)
moveSelectorPos (Pile, _, _) dir 
  | dir == right = (EndingStacks, 0, 0)
  | dir == down  = (GameField,    0, 0)
moveSelectorPos (EndingStacks, 0, _) dir 
  | dir == left  = (Pile, 0, 0) 
moveSelectorPos (EndingStacks, x, _) dir 
  | dir == down  = (GameField, x + 3, 0)
moveSelectorPos (region, x, y) (dx, dy) = (region, x + dx, y + dy)
