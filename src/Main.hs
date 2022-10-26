import VoorbeeldModule (hoi, hallo)
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

main :: IO ()
main = putStrLn hoi

-- === RenderFuncties ===

-- renderCard :: Card -> Picture

--main :: IO ()
--main = play white fps initBoard render handleInput step
