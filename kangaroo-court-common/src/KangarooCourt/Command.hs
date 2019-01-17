{-# LANGUAGE DeriveGeneric #-}

module KangarooCourt.Command where 
    import Data.Text (Text)
    import Data.Aeson
    import KangarooCourt.Card
    import GHC.Generics

    newtype PlayerId = PlayerId { unPlayerId :: Text }
        deriving (Show, Read, Eq, Ord, Generic)
    
    instance FromJSON PlayerId
    instance ToJSON PlayerId
    
    newtype Explanation = Explanation { unExplanation :: Text }
        deriving (Show, Read, Eq, Ord, Generic)

    instance FromJSON Explanation
    instance ToJSON Explanation

    -- | All the various events that can happen in a game, wrapped up in a newtype
    data GameEvent 
        = DealAnimalCard AnimalCard
        -- ^ Deal an animal card (at the start of the game)
        | DealJobCard JobCard
        -- ^ Deal a job card (at the start of the game)
        | DealRoutineCard RoutineCard
        -- ^ Deal a routine card (as part of the round)
        | PerformRoutine Explanation
        -- ^ Perform the routine as a player with the given explanation of how
        | AcceptRoutine
        -- ^ Accept that the player has performed their routine
        | CallButWait Explanation
        -- ^ Call a "but wait" with the given explanation of why
        | ValidateButWait 
        -- ^ The given player validates a "but wait" as being legitimate
        | RejectButWait
        -- ^ The given player rejects a "but wait" as being illegitimate
        | GuessAnimal AnimalCard
        -- ^ The given player tries to guess their animal
        | GameOver
        -- ^ The game has ended
        deriving (Show, Read, Eq, Ord, Generic)

    instance FromJSON GameEvent
    instance ToJSON GameEvent

    -- | Class of monads with the ability to "play" a game of Kangaroo Court
    class (Monad m) => MonadKangarooCourtPlaying m where
        getGameEvent :: m (PlayerId, GameEvent)
        -- ^ Get the most recent event in the game

    isGameOver GameOver = True
    isGameOver _ = False