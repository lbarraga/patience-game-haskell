module PatienceLogica where

import Types
import Cards (placeholderCard, showCard)

-- === Constanten == 

-- | Alle kaarten
allCards :: [Card]
allCards = [(cardType, cardValue, Hidden) | cardType <- [Clubs .. Spades], cardValue <- [Ace .. King]]

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


rotateStackNTimes :: Stack -> Int -> Stack
rotateStackNTimes [] _     = []
rotateStackNTimes l 0      = l
rotateStackNTimes (x:xs) n = rotateStackNTimes (xs ++ [x]) (n - 1)


-- show last card in a stack
showLast :: Stack -> Stack
showLast [] = [placeholderCard]
showLast stack = init stack ++ [showCard $ last stack]

