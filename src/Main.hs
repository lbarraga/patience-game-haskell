import PatienceLogica (initGame)
import Cards (placeholderCard, allCardsShown, isVisible, canPerformMovement)
import GameLogica (move, canPlaceSelector, handleGameSelection, deselect, rotatePile, canSelectorMove)
import Types

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Data.Maybe (fromJust, isNothing, catMaybes, fromMaybe)
import GHC.IO (unsafePerformIO)
import Types (Coordinate, Region (EndingStacks))
import Data.Tuple (uncurry)

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

-- pxlWidth:  De breedte van het spel in pixels.
-- pxlHeight: De hoogte van het spel in pixels.
pxlWidth, pxlHeight :: Int
pxlWidth = 800
pxlHeight = 700

-- De breedte van een kaart foto
pxlImageWidth :: Float
pxlImageWidth = 100

-- pxlVertStackInset: aantal pixels een kaart naar onder geschoven wordt na zijn voorganger op de stapel
-- pxlHorStackInset:  aantal pixels tussen de stapels op het speelveld
pxlVertStackInset, pxlHorStackInset :: Float
pxlVertStackInset = 25
pxlHorStackInset = 5

pxlFullHorInset :: Float
pxlFullHorInset = pxlImageWidth + pxlHorStackInset

cardsBetweenPileAndEndingStacks :: Float
cardsBetweenPileAndEndingStacks = 2

xOffset, yOffset :: Float
xOffset = -300
yOffset = 0

endingStacksXOffset :: Float
endingStacksXOffset = xOffset + (pxlImageWidth + pxlHorStackInset) * (cardsBetweenPileAndEndingStacks + 1)

spaceBetweenGameAndUpperField :: Float
spaceBetweenGameAndUpperField = 150

upperFieldYOffset :: Float
upperFieldYOffset = yOffset + spaceBetweenGameAndUpperField

--Initiele positie van het Gloss venster.
windowPosition :: (Int, Int)
windowPosition = (200, 80)

-- De plaats waar de fotos opgeslagen zijn.
assetFolder :: String
assetFolder = "lib/assets"

-- De pictures van een aantal standaartelementen uit het spel
backPicture, placeholderPicture, selectorPicture, selectedPicture :: Picture
backPicture        = png $ assetFolder ++ "/" ++ "back.png"
placeholderPicture = png $ assetFolder ++ "/" ++ "placeholder.png"
selectorPicture    = png $ assetFolder ++ "/" ++ "selector.png"
selectedPicture    = png $ assetFolder ++ "/" ++ "selected.png"

cardLookupTable :: [(Card, Picture)]
cardLookupTable = (placeholderCard, placeholderPicture) : zip allCardsShown (map (png . toPath) allCardsShown)

-- Achtergrond kleur van het gloss venster
backgroundColor :: Color
backgroundColor = makeColorI 221 160 221 1 -- Plum: Paars-achtige haskell kleur

-- Het Gloss venster
window :: Display
window = InWindow "Patience" (pxlWidth, pxlHeight) windowPosition

-- =============================================================
-- ||                      HulpFuncties                       ||
-- =============================================================

png :: String -> Picture
png = fromJust . unsafePerformIO . loadJuicyPNG

-- Zoek de picture van een kaart op. backpicture wanneer kaart niet gevonden.
cardLookup :: Card -> Picture
cardLookup findCard = fromMaybe backPicture (lookup findCard cardLookupTable)

-- Zet de zelfgedefinieerde `Coordinate` om in een `GlossCoordinate`
coordinate2Gloss :: Coordinate -> GlossCoordinate
coordinate2Gloss (EndingStacks, x, _) = (endingStacksXOffset + pxlFullHorInset * fromIntegral x, upperFieldYOffset)
coordinate2Gloss (GameField, x, y)    = (xOffset             + pxlFullHorInset * fromIntegral x, yOffset + (-pxlVertStackInset) * fromIntegral y)
coordinate2Gloss (Pile, _, _)         = (xOffset, upperFieldYOffset)

translateToGloss :: Coordinate -> Picture -> Picture
translateToGloss = uncurry translate . coordinate2Gloss

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


-- ===============================================================================
-- ||                              RENDERFUNCTIES                               ||
-- ===============================================================================

-- Render de game
renderBoard :: Board -> Picture
renderBoard Board{ gameStacks = gs, pile = p, endingStacks = es } = pictures [speelVeld, endingStacks, pile]
    where speelVeld    = translate xOffset             yOffset           (renderGameStacks   gs)
          pile         = translate xOffset             upperFieldYOffset (renderPile         p )
          endingStacks = translate endingStacksXOffset upperFieldYOffset (renderEndingstacks es)

-- Render de selector
renderSelector :: Selector -> Picture
renderSelector Selector{position = pos, selected = maybePos} = pictures [renderSelectorPos pos, renderSelectedPos maybePos]

-- Render de positie van de selector
renderSelectorPos :: Coordinate -> Picture
renderSelectorPos pos = translateToGloss pos selectorPicture

-- Render de selected van de selector
renderSelectedPos :: Maybe Coordinate -> Picture
renderSelectedPos Nothing    = blank
renderSelectedPos (Just pos) = translateToGloss pos selectedPicture

-- Render het bord
renderGame :: Game -> Picture
renderGame Game{board = b, selector = s} = pictures [renderBoard b, renderSelector s]

-- Render de stapels over heel de game
renderGameStacks :: [Stack] -> Picture
renderGameStacks = mapBeforeTranslate renderStack pxlFullHorInset 0

-- Render een enkele stapel
renderStack :: Stack -> Picture
renderStack = mapBeforeTranslate cardLookup 0 (-pxlVertStackInset)

-- Render de ending stapels
renderEndingstacks :: [Stack] -> Picture
renderEndingstacks = mapBeforeTranslate renderPile pxlFullHorInset 0

-- toPicture: een functie die een bepaald waarde omzet naar een picture
-- dx, dy: de 
mapBeforeTranslate :: (a -> Picture) -> Float -> Float -> [a] -> Picture
mapBeforeTranslate toPicture dx dy = pictures . translateCumulative dx dy . map toPicture


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




















