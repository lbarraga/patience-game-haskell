import VoorbeeldModule (hoi, hallo)
import Types
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Data.Maybe (fromJust, isNothing)

-- ===============================================================
-- Main module: deze module heeft als doel de gegenereerde
-- objecten te renderen met Gloss, en is het ingangspunt van 
-- de applicatie. Ook zijn hier verschillende constanten 
-- gedefinieerd met betrekking tot het uitzicht van het spel
-- ===============================================================

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
pxlVerticalStackInset :: Float
pxlVerticalStackInset = 25

-- aantal pixels tussen de stapels op het speelveld
pxlHorizontalStackInset :: Float
pxlHorizontalStackInset = 5

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

-- TODO move
-- | Alle kaarten
allCards :: [Card]
allCards = [(cardType, cardValue, Hidden) | cardType <- [Clubs .. Spades], cardValue <- [Ace .. King]]

-- Aantal ending stacks
aantalEndingStacks :: Int
aantalEndingStacks = 4

-- TODO move 
-- de placeholder kaart
placeholderCard :: Card
placeholderCard = (NoneType, NoneValue, NoneStatus)

-- TODO move
-- Het aantal oplopende stapels op het speelveld
aantalSpeelVeldStapels :: Int
aantalSpeelVeldStapels = 7

-- TODO move
-- Het aantal kaarten op het speelveld
aantalSpeelVeldKaarten :: Int
aantalSpeelVeldKaarten = n*(n+1) `div` 2 -- som van eerste n getallen 
    where n = aantalSpeelVeldStapels

-- TODO move
-- De initiele kaarten op het SpeelVeld
initGameField :: [Stack]
initGameField = makeGameField (take aantalSpeelVeldKaarten allCards) aantalSpeelVeldStapels

-- TODO move 
-- De initiele kaarten op de pile (kaarten die overgehouden zijn na het nemen van de speelVeldKaarten)
initPile :: Stack
initPile = showLast $ drop aantalSpeelVeldKaarten allCards

-- TODO move
-- de initiele kaarten op de endingstacks
initEndingStacks :: [Stack]
initEndingStacks = replicate aantalEndingStacks [placeholderCard]

-- TODO move
-- Initiele opstelling van het bord
initBoard :: Board
initBoard = Board {
    gameStacks = initGameField,
    pile = initPile,
    endingStacks = initEndingStacks
}

-- TODO move
-- Initiele staat van de selector.
initSelector :: Selector
initSelector = Selector {position = (0, 0), selected = Nothing}

-- TODO move
-- Initiele opstelling van de game
initGame :: Game
initGame = Game {board = initBoard, selector = initSelector}


-- === HulpFuncties ===

-- Zoek de picture van een kaart op
lookUp :: [(Card, Picture)] -> Picture -> Card -> Picture
lookUp table defaultBackPicture findCard
  | isNothing picture = defaultBackPicture -- omgedraaide kaart wanneer niet gevonden
  | otherwise         = fromJust picture   -- picture van de kaart wanneer gevonden
  where picture = lookup findCard table -- ! lookup zonder hoofdletter uit Prelude !

-- TODO naar een andere file brengen
-- Maak stapels van oplopende grootte: van 1, 2, ..., tot grootte maxPile
makeGameField :: [Card] -> Int -> [Stack]
makeGameField cards 0 = []
makeGameField cards maxPile = makeGameField (drop maxPile cards) (maxPile - 1) ++ [showLast $ take maxPile cards]

-- TODO andere file
-- show last card in a stack
showLast :: Stack -> Stack
showLast stack = init stack ++ [(t, v, Visible)] where (t, v, _) = last stack


-- Krijgt een kaart en geeft het pad van de foto van die kaart terug.
toPath :: Card -> String
toPath (cardType, cardValue, _) = assetFolder ++ "/" ++ fotoMap ++ "/" ++ image
    where fotoMap = show cardType ++ "s"
          image   = show cardType ++ "-" ++ show cardValue ++ ".png"

-- Verschuift de eerste picture niet, de tweede met (x, y), de derde met (2x, 2y), ...
transLateCumulative :: Float -> Float -> [Picture] -> [Picture]
transLateCumulative _ _ [] = []
transLateCumulative x y (pic:rest) = pic : transLateCumulative x y (map (translate x y) rest)

-- === RenderFuncties ===

-- Render een kaart
renderCard :: Card -> IO (Maybe Picture)
renderCard (NoneType, NoneValue, NoneStatus) = loadJuicyPNG $ assetFolder ++ "/placeholder.png"
renderCard (_, _, Hidden)                    = loadJuicyPNG $ assetFolder ++ "/back.png"
renderCard card                              = loadJuicyPNG $ toPath card

-- Render een enkele stapel
renderStack :: (Card -> Picture) -> Stack -> Picture
renderStack cardLookup stack = pictures $ transLateCumulative 0 (-pxlVerticalStackInset) cardPictures
    where cardPictures = map cardLookup stack

-- Render de game
renderBoard :: (Card -> Picture) -> Board -> Picture
renderBoard cardLookup Board{ gameStacks = gs, pile = p, endingStacks = es } = pictures [speelVeld, endingStacks, pile]
    where speelVeld    = translate (-300) 0   (renderGameStacks cardLookup gs) -- TODO constanten weg
          endingStacks = translate 15     150 (renderEndingstacks cardLookup es) -- TODO -85 = -400 + 3*100 + 3*hInsets
          pile         = translate (-300) 150 (renderPile cardLookup p)

-- Render de selector
renderSelector :: Selector -> Picture
renderSelector = undefined

-- Render het bord
renderGame :: (Card -> Picture) -> Game -> Picture
renderGame cardLookup Game{board = b, selector = s} = renderBoard cardLookup b

-- Render de stapels over heel de game
renderGameStacks :: (Card -> Picture) -> [Stack] -> Picture
renderGameStacks cardLookup stacks = pictures $ transLateCumulative dx 0 stackPictures
    where stackPictures = map (renderStack cardLookup) stacks
          dx = pxlImageWidth + pxlHorizontalStackInset

-- Render de ending stapels
renderEndingstacks :: (Card -> Picture) -> [Stack] -> Picture
renderEndingstacks cardLookup stacks = pictures $ transLateCumulative dx 0 stackPictures
    where stackPictures = map (renderPile cardLookup) stacks
          dx = pxlImageWidth + pxlHorizontalStackInset -- TODO dup weghalen

-- Render de afhaalstapel
renderPile :: (Card -> Picture) -> Stack -> Picture
renderPile cardLookup stack = cardLookup $ last stack

-- stap in de game
step :: Float -> Game -> Game
step _ game = game

handleInput :: Event -> Game -> Game
handleInput _ game = game

main :: IO ()
main = do
    let allShowCards = placeholderCard : [(cardType, cardValue, Visible) | cardType <- [Clubs .. Spades], cardValue <- [Ace .. King]]
    print initGameField
    print aantalSpeelVeldKaarten
    cards <- mapM renderCard allShowCards 
    defaultBackPicture <- fmap fromJust (renderCard (Hearts, Ace, Hidden))
    let cardPictures = map fromJust cards
    let cardPictureLookupTable = zip allShowCards cardPictures
    let cardLookup = lookUp cardPictureLookupTable defaultBackPicture
    play window backgroundColor fps initGame (renderGame cardLookup) handleInput step




















