import VoorbeeldModule (hoi, hallo)
import Types
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss

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

-- Initiele opstelling van de game
initGame :: Game
initGame = undefined

main :: IO ()
main = putStrLn hoi

-- === RenderFuncties ===

-- Render een kaart
renderCard :: Card -> Picture
renderCard = undefined

-- Render een enkele stapel
renderStack :: Stack -> Picture
renderStack = undefined

-- Render de game
renderGame :: Game -> Picture
renderGame = undefined

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

--main :: IO ()
--main = play white fps initGame renderGame handleInput step
