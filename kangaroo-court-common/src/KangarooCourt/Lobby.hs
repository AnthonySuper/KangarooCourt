{-# LANGUAGE DeriveGeneric #-}

module KangarooCourt.Lobby where
    import Data.Text (Text)
    import Data.Map.Strict (Map)
    import GHC.Generics
    import Data.Aeson
    import KangarooCourt.Command
    import KangarooCourt.Deck

    data Lobby
        = Lobby
        { name :: Text
        , players :: [PlayerId]
        , deck :: Deck 
        } deriving (Show, Read, Eq, Generic)

    instance ToJSON Lobby
    instance FromJSON Lobby

    data LobbyMessage a
        = GameStarted a
        | ChatMessage PlayerId Text
        deriving (Show, Read, Eq, Generic)

    instance (ToJSON a) => ToJSON (LobbyMessage a)
    instance (FromJSON a) => FromJSON (LobbyMessage a)

    -- | Type alias for external (user-sent) lobby messages
    type LobbyMessageE =
        LobbyMessage ()
    
    data LobbyCommand
        = AppendDeck Deck
        | SendChat Text
        | StartGame
        deriving (Show, Read, Eq, Generic)

    instance ToJSON LobbyCommand
    instance FromJSON LobbyCommand
    
    data BrokerCommand
        = CreateLobby Text Lobby
        | JoinLobby Text
        | ListLobbies
        deriving (Show, Read, Eq, Generic)

    instance ToJSON BrokerCommand
    instance FromJSON BrokerCommand

    data BrokerResponse a
        = JoinedLobby Text a
        | NameTaken Text
        | NotFound Text
        | GetLobbyList (Map Text Lobby)
        | BadMessage
        deriving (Show, Read, Eq, Generic)
    
    instance (ToJSON a) 
        => ToJSON (BrokerResponse a)
    instance (FromJSON a) 
        => FromJSON (BrokerResponse a)
    type BrokerResponseE 
        = BrokerResponse ()