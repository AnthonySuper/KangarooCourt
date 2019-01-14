{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module KangarooCourt.Deck where
    import GHC.Generics
    import Data.Aeson
    import KangarooCourt.Card

    data Deck
        = Deck
        { animalCards :: [AnimalCard]
        , routineCards :: [RoutineCard]
        , jobCards :: [JobCard] } deriving (Show, Read, Eq, Generic)

    instance ToJSON Deck
    instance FromJSON Deck
    
    instance Semigroup Deck where
        (Deck a e j) <> (Deck a' e' j') =
            Deck (a <> a') (e <> e') (j <> j')

    instance Monoid Deck where
        mempty = Deck mempty mempty mempty
    
    drawAnimalCard :: Deck -> (Maybe AnimalCard, Deck)
    drawAnimalCard (Deck (a:as) e j) =
        (Just a, Deck as e j)
    drawAnimalCard d = (Nothing, d)

    drawRoutineCard :: Deck -> (Maybe RoutineCard, Deck)
    drawRoutineCard (Deck a (e:es) j) =
        (Just e, Deck a es j)
    drawRoutineCard d = (Nothing, d)

    drawJobCard :: Deck -> (Maybe JobCard, Deck)
    drawJobCard (Deck a e (j:js)) =
        (Just j, Deck a e js)
    drawJobCard d = (Nothing, d)