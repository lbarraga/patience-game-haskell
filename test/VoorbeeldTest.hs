import Test.Hspec

import Cards (allCardsShown, isVisible, placeholderCard)
import BoardLogica (aantalSpeelVeldKaarten, initPile, initGameField, initEndingStacks)

main :: IO ()
main = hspec $ do 

    it "Er zijn 52 kaarten" $ do
        length allCardsShown `shouldBe` 52
    it "Elke kaart is visible" $ do
        all isVisible allCardsShown `shouldBe` True
    it "InitGameField maakt weldegelijk `aantalSpeelVeldKaarten` aantal stapels" $ do
        (sum . map length) initGameField `shouldBe` aantalSpeelVeldKaarten
    it "De pile bestaat uit de overige kaarten" $ do
        length initPile `shouldBe` 52 - aantalSpeelVeldKaarten
    it "De endingstacks bevatten initieel enkel de placeholdercard" $ do
        all (==[placeholderCard]) initEndingStacks `shouldBe` True
