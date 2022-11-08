import PatienceLogica
import Types
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Data.Maybe (fromJust, isNothing, catMaybes, fromMaybe)
import Control.Exception
import Control.Concurrent (waitQSem)
import Data.Bool (Bool)
import GHC.IO (unsafePerformIO)

-- ===============================================================
-- Main module: deze module heeft als doel de gegenereerde
-- objecten te renderen met Gloss, en is het ingangspunt van 
-- de applicatie. Ook zijn hier verschillende constanten 
-- gedefinieerd met betrekking tot het uitzicht van het spel
-- ===============================================================

-- Een gloss-coordinaat
type GlossCoordinate = (Float, Float)

-- === Constanten ===

-- Framerate van het spel.
fps :: Int
fps = 60

-- De breedte van het spel in pixels.
pxlWidth :: Int
pxlWidth = 800

-- De hoogte van het spel in pixels.
pxlHeight :: Int
pxlHeight = 700

-- De breedte van een kaart foto
pxlImageWidth :: Float
pxlImageWidth = 100

-- aantal pixels een kaart naar onder geschoven wordt na zijn voorganger op de stapel
pxlVertStackInset :: Float
pxlVertStackInset = 25

-- aantal pixels tussen de stapels op het speelveld
pxlHorStackInset :: Float
pxlHorStackInset = 5

--Initiele positie van het Gloss venster.
windowPosition :: (Int, Int)
windowPosition = (200, 80)

-- De plaats waar de fotos opgeslagen zijn.
assetFolder :: String
assetFolder = "lib/assets"

backPicture, placeholderPicture, selectorPicture, selectedPicture :: Picture
backPicture        = png $ assetFolder ++ "/" ++ "back.png"
placeholderPicture = png $ assetFolder ++ "/" ++ "placeholder.png"
selectorPicture    = png $ assetFolder ++ "/" ++ "selector.png"
selectedPicture    = png $ assetFolder ++ "/" ++ "selected.png"

allCardsShown :: [Card]
allCardsShown = [(cardType, cardValue, Visible) | cardType <- [Clubs .. Spades], cardValue <- [Ace .. King]]

cardLookupTable :: [(Card, Picture)]
cardLookupTable = (placeholderCard, placeholderPicture) : zip allCardsShown (map (png . toPath) allCardsShown)

-- Achtergrond kleur van het gloss venster
backgroundColor :: Color
backgroundColor = makeColorI 221 160 221 1 -- Plum: Paars-achtige haskell kleur

-- Het Gloss venster
window :: Display
window = InWindow "Patience" (pxlWidth, pxlHeight) windowPosition

-- === HulpFuncties ===

png :: String -> Picture
png = fromJust . unsafePerformIO . loadJuicyPNG

-- Zoek de picture van een kaart op. backpicture wanneer kaart niet gevonden.
cardLookup :: Card -> Picture
cardLookup findCard = fromMaybe backPicture (lookup findCard cardLookupTable)

-- Zet de zelfgedefinieerde `Coordinate` om in een `GlossCoordinate`
coordinate2Gloss :: Coordinate -> GlossCoordinate
coordinate2Gloss (GameField, x, y)    = ((-300) + (pxlImageWidth + pxlHorStackInset) * fromIntegral x, -pxlVertStackInset * fromIntegral y)
coordinate2Gloss (EndingStacks, x, _) = (15 + (pxlImageWidth + pxlHorStackInset) * fromIntegral x, 150)
coordinate2Gloss (Pile, _, _)         = (-300, 150)

-- Krijgt een kaart en geeft het pad van de foto van die kaart terug.
toPath :: Card -> String
toPath (cardType, cardValue, _) = assetFolder ++ "/" ++ fotoMap ++ "/" ++ image
    where fotoMap = show cardType ++ "s"
          image   = show cardType ++ "-" ++ show cardValue ++ ".png"

-- f [a1, a2, a3, ...] -> [a1, f a2, f (f a3), ...]
cumulateF :: (a -> a) -> [a] -> [a] 
cumulateF f [] = []
cumulateF f (x:xs) = x : cumulateF f (map f xs)

-- Verschuift de eerste picture niet, de tweede met (x, y), de derde met (2x, 2y), ...
translateCumulative :: Float -> Float -> [Picture] -> [Picture]
translateCumulative x y = cumulateF (translate x y)

-- Hulpfunctie die nagaat of een bepaalde toets is ingedrukt.
isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Down _ _) = k1 == k2
isKey _  _                                   = False

-- Move de selector 
moveSelector :: Selector -> Direction -> Selector
moveSelector s@Selector{position = p} dir = s{position = moveSelectorPos p dir}

-- move de selector van de game
move :: Game -> Direction -> Game
move g@Game{selector = s} dir = g{selector = moveSelector s dir}

-- Is een kaart omgedraaid
isCardVisible :: Card -> Bool
isCardVisible (_, _, status) = status == Visible

-- TODO move
selectCard :: Game -> Game
selectCard g@Game{selector = s@Selector{position = p}} = g{selector = s{selected = Just p}}

-- TODO move
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

-- Of een kaart geselecteerd kan worden of niet. 
canSelect :: Coordinate -> Game -> Bool
canSelect co = isCardVisible . getCardFromCo co

-- c1: de kaart die verplaatst word
-- c2: de kaart waarop c1 geplaatst wordt.
-- region: de `Region` van c2 
-- returns: Of kaart c1 op kaart c2 geplaatst kan worden
canPerformMovement :: Region -> Card -> Card -> Bool
canPerformMovement Pile         c1 c2 = False -- Kan niet op de Pile plaatsen. 
canPerformMovement EndingStacks c1 c2 = (c2 == placeholderCard && isAce  c1) || (getSuit  c1 == getSuit  c2 && c2 `isOneLess` c1)
canPerformMovement GameField    c1 c2 = (c2 == placeholderCard && isKing c1) || (getColor c1 /= getColor c2 && isCardVisible c2 && c1 `isOneLess` c2)

-- TODO kaartmodule
isOneLess :: Card -> Card -> Bool
isOneLess (_, v1, _) (_, v2, _) = fromEnum v1 == fromEnum v2 - 1

-- TODO kaartModule
isAce :: Card -> Bool
isAce (_, v, _) = v == Ace

-- TODO kaartModule
isKing :: Card -> Bool
isKing (_, v, _) = v == King

rotateStackNTimes :: Stack -> Int -> Stack
rotateStackNTimes [] _     = []
rotateStackNTimes l 0      = l
rotateStackNTimes (x:xs) n = rotateStackNTimes (xs ++ [x]) (n - 1)

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

-- TODO move kaartModule
getColor :: Card -> CardColor
getColor (Hearts  , _, _) = Red
getColor (Diamonds, _, _) = Red
getColor _ = Black

-- TODO move kaartModule
getSuit :: Card -> CardType
getSuit (v, _, _) = v

-- TODO move kaartModule
getValue :: Card -> Int
getValue (_, value, _) = fromEnum value

-- TODO move naar board Module top level
moveSubStack :: Coordinate -> Coordinate -> Game -> Game
moveSubStack from to g@Game{board = b} = g{board = newBoard}
    where newBoard = boardAfterPlacingStack to removed removedBoard
          (removed, removedBoard) = takeStackAction from b 

-- TODO board
-- Verwijder een substapel kaarten van het bord,
-- geeft de substapel terug en het bord na de verwijder actie
takeStackAction :: Coordinate -> Board -> (Stack, Board)
takeStackAction co b = (takeStack co b, boardAfterRemove co b)

-- TODO board
takeStack :: Coordinate -> Board -> Stack
takeStack (Pile, _, _)         b = [last $ pile b]
takeStack (EndingStacks, x, _) b = [last $ endingStacks b !! x]
takeStack (GameField, x, y)    b = drop y (gameStacks b !! x)

-- TODO board
boardAfterRemove :: Coordinate -> Board -> Board
boardAfterRemove (Pile, _, _)         b = b{pile = (showLast . init) $ pile b}
boardAfterRemove (EndingStacks, x, _) b = b{endingStacks = replaceStack x stacks newEndingStack}
    where newEndingStack = (showLast . init) (stacks !! x) -- laatste is weg
          stacks = endingStacks b
boardAfterRemove (GameField, x, y)    b = b{gameStacks = replaceStack x stacks newGameStack}
    where newGameStack = (showLast . take y) (stacks !! x) -- alle kaarten vanaf co y zijn weg 
          stacks = gameStacks b

-- TODO board
checkPlaceholder :: Stack -> Stack
checkPlaceholder (x:xs)
  | x == placeholderCard = xs
  | otherwise            = x:xs

-- TODO board
placeOnStack :: [Stack] -> Int -> Stack -> [Stack]
placeOnStack stacks replaceIndex stack = replaceStack replaceIndex stacks newEndingStack
    where newEndingStack = checkPlaceholder (stacks !! replaceIndex ++ stack)

-- TODO board
boardAfterPlacingStack :: Coordinate ->  Stack -> Board -> Board
boardAfterPlacingStack (EndingStacks, x, _) s b = b{endingStacks = placeOnStack (endingStacks b) x s}
boardAfterPlacingStack (GameField, x, _)    s b = b{gameStacks   = placeOnStack (gameStacks   b) x s}

-- TODO board
replaceStack :: Int -> [Stack] -> Stack -> [Stack]
replaceStack i stacks with = before ++ [with] ++ after
    where (before, _:after) = splitAt i stacks

-- ===============================================================================
-- ||                              RENDERFUNCTIES                               ||
-- ===============================================================================

-- Render de game
renderBoard :: Board -> Picture
renderBoard Board{ gameStacks = gs, pile = p, endingStacks = es } = pictures [speelVeld, endingStacks, pile]
    where speelVeld    = translate (-300) 0   (renderGameStacks gs)   -- TODO constanten weg
          endingStacks = translate 15     150 (renderEndingstacks es) -- TODO -85 = -400 + 3*100 + 3*hInsets
          pile         = translate (-300) 150 (renderPile p)

-- Render de selector
renderSelector :: Selector -> Picture
renderSelector Selector{position = pos, selected = maybePos} = pictures [renderSelectorPos pos, renderSelectedPos maybePos]

-- Render de positie van de selector
renderSelectorPos :: Coordinate -> Picture
renderSelectorPos pos = Translate x y selectorPicture
    where (x, y) = coordinate2Gloss pos

-- Render de selected van de selector
renderSelectedPos :: Maybe Coordinate -> Picture
renderSelectedPos Nothing    = blank
renderSelectedPos (Just pos) = Translate x y selectedPicture
    where (x, y) = coordinate2Gloss pos

-- Render het bord
renderGame :: Game -> Picture
renderGame Game{board = b, selector = s} = pictures [renderBoard b, renderSelector s]

-- Render de stapels over heel de game
renderGameStacks :: [Stack] -> Picture
renderGameStacks stacks = renderYeet stacks renderStack (pxlImageWidth + pxlHorStackInset) 0

-- Render een enkele stapel
renderStack :: Stack -> Picture
renderStack stack = renderYeet stack cardLookup 0 (-pxlVertStackInset)

renderYeet :: [a] -> (a -> Picture) -> Float -> Float -> Picture
renderYeet l f dx dy = pictures $ translateCumulative dx dy pics
    where pics = map f l

-- Render de ending stapels
renderEndingstacks :: [Stack] -> Picture
renderEndingstacks stacks = pictures $ translateCumulative dx 0 stackPictures
    where stackPictures = map renderPile stacks -- Alle pictures in de map
          dx = pxlImageWidth + pxlHorStackInset -- TODO dup weghalen

-- Render de afhaalstapel
renderPile :: Stack -> Picture
renderPile = cardLookup . last

-- stap in de game
step :: Float -> Game -> Game
step _ game = game

handleInput :: Event -> Game -> Game
handleInput ev g@Game{board = b, selector = s} 
  | isKey KeyDown  ev && canSelectorMove g down   = move g down
  | isKey KeyUp    ev && canSelectorMove g up     = move g up
  | isKey KeyLeft  ev && canSelectorMove g left   = move g left
  | isKey KeyRight ev && canSelectorMove g right  = move g right
  | isKey KeySpace ev && not (canPlaceSelector g) = deselect g
  | isKey KeySpace ev && canPlaceSelector g       = handleGameSelection g
  | isKey KeyEnter ev = rotatePile g
handleInput _ game = game

main :: IO ()
main = play window backgroundColor fps initGame renderGame handleInput step




















