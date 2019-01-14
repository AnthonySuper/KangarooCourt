{-# LANGUAGE DeriveGeneric #-}

module KangarooCourt.Card where
    import GHC.Generics
    import Data.Text (Text)
    import Data.Aeson

    newtype AnimalCard = AnimalCard { getAnimalCard :: Text }
        deriving (Show, Read, Eq, Ord, Generic)

    newtype JobCard = JobCard { getJobCard :: Text }
        deriving (Show, Read, Eq, Ord, Generic)

    newtype RoutineCard = RoutineCard { getRoutineCard :: Text }
        deriving (Show, Read, Eq, Ord, Generic)

    instance ToJSON AnimalCard
    instance FromJSON AnimalCard
    instance ToJSON JobCard
    instance FromJSON JobCard
    instance ToJSON RoutineCard
    instance FromJSON RoutineCard
    