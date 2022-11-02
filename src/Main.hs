import VoorbeeldModule (hoi, hallo)
import Types
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Data.Maybe (fromJust)

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
pxlWidth = 200

-- De hoogte van het spel in pixels.
pxlHeight :: Int
pxlHeight = 100

--Initiele positie van het Gloss venster.
windowPosition :: (Int, Int)
windowPosition = (200, 200)

-- De plaats waar de fotos opgeslagen zijn.
assetFolder :: String
assetFolder = "lib/assets"

-- Het Gloss venster
window :: Display
window = InWindow "Patience" (pxlWidth, pxlHeight) windowPosition

-- | Alle kaarten
allCards :: [Card]
allCards = [(cardType, cardValue, Hidden) | cardType <- [Clubs .. Spades], cardValue <- [Ace .. King]]

-- Het aantal oplopende stapels op het speelveld
aantalSpeelVeldStapels :: Int
aantalGameVeldStapels = 7

-- Het aantal kaarten op het speelveld
aantalSpeelVeldKaarten :: Int
aantalSpeelVeldKaarten = n*(n-1) `div` 2 -- som van eerste n getallen 
    where n = aantalGameVeldStapels

-- Initiele opstelling van het bord
initBoard :: Board
initBoard = Board {
    gameStacks = makeGameStacks speelVeldKaarten aantalSpeelVeldStapels, -- TODO constanten weghalen
    pile = pileKaarten,
    endingStacks = [[], [], [], []]
} where speelVeldKaarten = take aantalSpeelVeldKaarten allCards
        pileKaarten = drop aantalSpeelVeldKaarten allCards

-- Initiele staat van de selector.
initSelector :: Selector
initSelector = Selector {position = (0, 0), selected = Nothing}

-- Initiele opstelling van de game
initGame :: Game
initGame = Game {board = initBoard, selector = initSelector}


-- === HulpFuncties ===

-- Maak stapels van oplopende grootte: van 1, 2, ..., tot grootte maxPile
makeGameStacks :: [Card] -> Int -> [Stack]
makeGameStacks cards 0 = []
makeGameStacks cards maxPile = makeGameStacks (drop maxPile cards) (maxPile - 1) ++ [take maxPile cards]

-- Krijgt een kaart en geeft het pad van de foto van die kaart terug.
toPath :: Card -> String
toPath (cardType, cardValue, _) = assetFolder ++ "/" ++ fotoMap ++ "/" ++ image
    where fotoMap = show cardType ++ "s"
          image   = show cardType ++ "-" ++ show cardValue ++ ".png"

-- === RenderFuncties ===

-- Render een kaart
renderCard :: Card -> IO (Maybe Picture)
renderCard (_, _, Hidden) = loadJuicyPNG $ assetFolder ++ "/back.png"
renderCard card           = loadJuicyPNG $ toPath card

-- Render een enkele stapel
renderStack :: Stack -> Picture
renderStack = undefined

-- Render de game
renderGame :: Game -> Picture
renderGame = renderBoard . renderSelector

-- Render de selector
renderSelector :: Selector -> Picture
renderSelector = undefined

-- Render het bord
renderBoard :: Board -> Picture
renderBoard = undefined

-- Render de stapels over heel de game
renderGameStacks :: [Stack] -> Picture
renderGameStacks = undefined

-- Render de ending stapels
renderEndingstacks :: [Stack] -> Picture
renderEndingstacks = undefined

-- Render de afhaalstapel
renderPile :: Stack -> Picture
renderPile = undefined

-- stap in de game
step :: Float -> Game -> Game
step _ game = game

handleInput :: Event -> Game -> Game
handleInput _ game = game

main :: IO ()
main = do
    cards <- map renderCard allCards
    play window white fps initGame renderGame handleInput step




















