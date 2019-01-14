{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module KangarooCourt.DeckSpec where
    import Test.Hspec
    import Data.Generics.Product
    import KangarooCourt.Deck
    import KangarooCourt.Card
    import Control.Lens.Operators
    
    spec = drawSpec

    drawSpec = describe "drawing cards" $ do
        describe "drawing animal cards" animalDraw
        describe "drawing event cards" eventDraw
        describe "drawing job cards" jobDraw

    drawSpec' lens one f = do
        it "returns nothing on mempty" $
            fst (f mempty) `shouldBe` Nothing
        it "returns the head if it has one" $
            fst (f (mempty & lens .~ pure one)) `shouldBe` Just one

    animalDraw = 
        drawSpec' (field @"animalCards") (AnimalCard "") drawAnimalCard

    eventDraw =
        drawSpec' (field @"routineCards") (RoutineCard "") drawRoutineCard

    jobDraw = 
        drawSpec' (field @"jobCards") (JobCard "") drawJobCard

    