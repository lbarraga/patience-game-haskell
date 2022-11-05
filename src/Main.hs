import PatienceLogica
import Types
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Data.Maybe (fromJust, isNothing)
import Control.Exception
import Control.Concurrent (waitQSem)

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
pxlHeight = 500

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
windowPosition = (200, 200)

-- De plaats waar de fotos opgeslagen zijn.
assetFolder :: String
assetFolder = "lib/assets"

-- Achtergrond kleur van het gloss venster
backgroundColor :: Color
backgroundColor = makeColorI 221 160 221 1 -- Plum: Paars-achtige haskell kleur

-- Het Gloss venster
window :: Display
window = InWindow "Patience" (pxlWidth, pxlHeight) windowPosition

-- === HulpFuncties ===

-- Zoek de picture van een kaart op. backpicture wanneer kaart niet gevonden.
lookUp :: [(Card, Picture)] -> Picture -> Card -> Picture
lookUp table backPicture findCard 
  | isNothing picture = backPicture
  | otherwise         = fromJust picture 
  where picture = lookup findCard table -- ! lookup zonder hoofdletter uit Prelude !

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
-- Geeft aan of een kaart geselecteerd kan worden of niet.
-- Dit is het gavel als de kaart omgedraaid is en hij op het speelveld ligt.
canSelectCard :: Game -> Bool
canSelectCard g = canSelectPosition ((position . selector) g) g

canSelectPosition :: Coordinate -> Game -> Bool
canSelectPosition (Pile, _, _)         g = (not . null . pile . board) g
canSelectPosition (EndingStacks, x, _) g = (not . null) ((endingStacks . board) g !! x)
canSelectPosition (GameField, x, y)    g = isCardVisible card
    where card = cardStack !! y
          cardStack = (gameStacks  . board) g !! x

handleGameSelection :: Game -> Game
handleGameSelection g = handleSelection maybeSelected selectorPos g
      where maybeSelected = (selected . selector) g
            selectorPos   = (position . selector) g

-- Selecteerd een kaart indien er nog geen geselecteerd werd, 
-- verplaats anders alle kaarten vanaf selected naar de selector
handleSelection :: Maybe Coordinate -> Coordinate -> Game -> Game
handleSelection Nothing selectorPos = selectCard 
handleSelection (Just selectedPos) selectorPos = deselect . moveSubStack selectedPos selectorPos

deselect :: Game -> Game
deselect g@Game{selector = s} = g{selector = s{selected = Nothing}}

-- TODO move
-- Neem de kaart en alle bovenliggende op een bepaalde positie en plaats ze op een andere positie
-- Mogelijke verplaatsingen zijn
--  * GameField    -> GameField | EndingStacks
--  * Pile         -> GameField | EndingStacks
--  * EndingStacks -> GameField
canMoveRegion :: Region -> Region -> Bool
canMoveRegion EndingStacks EndingStacks = False -- Kan in principe maar waarom zou je.
canMoveRegion _ Pile = False -- Je kan geen kaarten op de Pile leggen.
canMoveRegion _ _ = True

getCardColor :: Card -> CardColor
getCardColor (Hearts  , _, _) = Red
getCardColor (Diamonds, _, _) = Red
getCardColor _ = Black

getCardValue :: Card -> Int
getCardValue (_, value, _) = fromEnum value

canMoveOnto :: Card -> Card -> Bool
canMoveOnto c1 c2 = colorsDifferent && oneLower
    where colorsDifferent = getCardColor c1 /= getCardColor c2
          oneLower = getCardValue c2 == getCardValue c2 - 1

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

boardAfterPlacingStack :: Coordinate ->  Stack -> Board -> Board
boardAfterPlacingStack (EndingStacks, x, _) s b = b{endingStacks = replaceStack x stacks newEndingStack }
    where newEndingStack = stacks !! x ++ s
          stacks = endingStacks b
boardAfterPlacingStack (GameField, x, _) s b = b{gameStacks = replaceStack x stacks newGameStack}
    where newGameStack = stacks !! x ++ s
          stacks = gameStacks b

replaceStack :: Int -> [Stack] -> Stack -> [Stack]
replaceStack i stacks with = before ++ [with] ++ after
    where (before, _:after) = splitAt i stacks

-- Render een enkele stapel
renderStack :: (Card -> Picture) -> Stack -> Picture
renderStack cardLookup stack = pictures $ translateCumulative 0 (-pxlVertStackInset) cardPictures
    where cardPictures = map cardLookup stack

-- Render de game
renderBoard :: (Card -> Picture) -> Board -> Picture
renderBoard cardLookup Board{ gameStacks = gs, pile = p, endingStacks = es } = pictures [speelVeld, endingStacks, pile]
    where speelVeld    = translate (-300) 0   (renderGameStacks cardLookup gs) -- TODO constanten weg
          endingStacks = translate 15     150 (renderEndingstacks cardLookup es) -- TODO -85 = -400 + 3*100 + 3*hInsets
          pile         = translate (-300) 150 (renderPile cardLookup p)

-- Render de selector
renderSelector :: (Picture, Picture) -> Selector -> Picture
renderSelector (selectorPic, selectedPic) Selector{position = pos, selected = maybePos} = pictures [renderSelectorPosition selectorPic pos, renderSelected selectedPic maybePos]

-- Render de positie van de selector
renderSelectorPosition :: Picture -> Coordinate -> Picture
renderSelectorPosition selectorPic pos = Translate x y selectorPic
    where (x, y) = coordinate2Gloss pos

-- Render de selected van de selector
renderSelected :: Picture -> Maybe Coordinate -> Picture
renderSelected _ Nothing = blank
renderSelected selectedPic (Just pos) = Translate x y selectedPic
    where (x, y) = coordinate2Gloss pos

-- Render het bord
renderGame :: (Card -> Picture) -> (Picture, Picture) -> Game -> Picture
renderGame cardLookup selectorPics Game{board = b, selector = s} = pictures [renderBoard cardLookup b, renderSelector selectorPics s]

-- Render de stapels over heel de game
renderGameStacks :: (Card -> Picture) -> [Stack] -> Picture
renderGameStacks cardLookup stacks = pictures $ translateCumulative dx 0 stackPictures
    where stackPictures = map (renderStack cardLookup) stacks
          dx = pxlImageWidth + pxlHorStackInset

-- Render de ending stapels
renderEndingstacks :: (Card -> Picture) -> [Stack] -> Picture
renderEndingstacks cardLookup stacks = pictures $ translateCumulative dx 0 stackPictures
    where stackPictures = map (renderPile cardLookup) stacks
          dx = pxlImageWidth + pxlHorStackInset -- TODO dup weghalen

-- Render de afhaalstapel
renderPile :: (Card -> Picture) -> Stack -> Picture
renderPile cardLookup stack = cardLookup $ last stack

-- stap in de game
step :: Float -> Game -> Game
step _ game = game

handleInput :: Event -> Game -> Game
handleInput ev g@Game{board = b, selector = s} 
  | isKey KeyDown  ev && canSelectorMove g down  = move g down
  | isKey KeyUp    ev && canSelectorMove g up    = move g up
  | isKey KeyLeft  ev && canSelectorMove g left  = move g left
  | isKey KeyRight ev && canSelectorMove g right = move g right
  | isKey KeySpace ev && canSelectCard g = handleGameSelection g
handleInput _ game = game

main :: IO ()
main = do
    let allShowCards = placeholderCard : [(cardType, cardValue, Visible) | cardType <- [Clubs .. Spades], cardValue <- [Ace .. King]] -- TODO weg
    maybeCardPictures <- mapM (loadJuicyPNG . toPath) allShowCards
    back         <- loadJuicyPNG $ assetFolder ++ "/" ++ "back.png"
    placeholder  <- loadJuicyPNG $ assetFolder ++ "/" ++ "placeholder.png"
    selectorPic  <- loadJuicyPNG $ assetFolder ++ "/" ++ "selector.png" 
    selectedPic  <- loadJuicyPNG $ assetFolder ++ "/" ++ "selected.png" 
    let cardPictures = map fromJust maybeCardPictures
    let cardPictureLookupTable = (placeholderCard, fromJust placeholder) : zip allShowCards cardPictures
    let cardLookup = lookUp cardPictureLookupTable (fromJust back)
    play window backgroundColor fps initGame (renderGame cardLookup (fromJust selectorPic, fromJust selectedPic)) handleInput step




















