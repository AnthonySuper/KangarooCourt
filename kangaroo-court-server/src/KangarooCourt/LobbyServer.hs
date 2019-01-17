{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module KangarooCourt.LobbyServer where
    import Data.Text (Text)
    import Data.Map.Strict (Map)
    import qualified Data.Map.Strict as Map
    import Data.Aeson
    import GHC.Generics
    import Control.Concurrent.STM.TVar
    import Control.Monad.STM
    import KangarooCourt.Deck
    import KangarooCourt.Command
    import Control.Concurrent.STM.TChan
    import Control.Concurrent.STM.TQueue
    import KangarooCourt.Broker
    import Data.Generics.Product
    import Control.Lens.Operators
    import KangarooCourt.Messaging
    import KangarooCourt.Lobby
    import KangarooCourt.BrokerMessage
    import Data.Maybe
    import Control.Monad
    import Control.Concurrent
    import Data.Foldable

    -- | Two-way communication for games.
    -- More specifically, this allows individuals to 
    -- *submit* GameEvents for possible writing, and to
    -- *read* GameEvents to actually change the game state.
    -- 
    -- An example is helpful in demonstrating its use:
    -- 
    -- > runPlayerClient :: (MonadSending IO t, MonadRecving IO t)
    -- >                => t -> GameCommunicator -> IO ()
    -- > runPlayerClient t c = do
    -- >     forkIO $ routeInput t (sendQ c)
    -- >     forever $ routeOutput t (recvC c)
    -- >
    -- > routeInput t q = 
    -- >     readGameMessage t >>= atomically . writeTQueue q
    -- >
    -- > routeOutput t c = do
    -- >     m <- readTChan c
    -- >     when (userCanSee m) $ sendGameMessage t m
    --
    data GameCommunicator
        = GameCommunicator
        { sendQ :: TQueue (PlayerId, GameEvent)
        -- ^ Send an event to the backend.
        -- The backend server will pop off the events, one
        -- at a time, and update the game's state and send out
        -- other events accordingly.
        , recvC :: TChan (PlayerId, GameEvent) 
        -- ^ Read the cannonical events from the backend.
        -- This channel should only ever be used for reading,
        -- unless you're the game-manager thread.
        --
        -- This channel contains a log of all the events
        -- that have canonically occured inside of the game.
        -- It is written to by the "game-manager" thread, which
        -- takes in event requests from the sendQ and forwards
        -- them to this channel if they are valid.
        -- If they would place the game in an illegal state,
        -- the game manager throws them away.
        -- Reading from this channel step-by-step allows the
        -- client to fully construct the state of the game.
        -- As a result, the thread that routes this channel to
        -- the actual socket __must__ be careful to censor game
        -- events which the client cannot know about!
        } deriving (Generic)

    -- | Type alias for internal lobby messages
    type LobbyMessageI =
        LobbyMessage GameCommunicator
    
    type BrokerEntry'
        = BrokerEntry Lobby LobbyMessageI

    type BrokerResponseI 
        = BrokerResponse BrokerEntry'

    -- | Type alias for the broker type used in this module
    -- More specifically, it's a broker with 'Data.Text.Text'
    -- as its keys, 'KangarooCourt.Lobby.Lobby' as its 
    -- lobby information, and 'LobbyMessageI' as its interal
    -- messaging type.
    type GameBroker =
        Broker Text Lobby LobbyMessageI

    -- | Type alias for the broker entry type used in this
    -- module. More specifically, it's a BrokerEntry with 
    -- 'KangarooCourt.Lobby.Lobby' as its lobby information,
    -- and 'LobbyMessageI' as its internal messaging type.
    type GameBrokerEntry =
        BrokerEntry Lobby LobbyMessageI

    -- | Add the given player to the lobby's list of users.
    -- Note that this function prepends it.
    addLobbyUser :: PlayerId -> Lobby -> Lobby
    addLobbyUser pid =
        field @"players" %~ (pid :)

    -- | Add a user to the lobby's BrokerEntry and
    -- obtain a "cloned" BrokerEntry that can be used
    -- to read messages from. More specifically, the
    -- cloned BrokerEntry will have the same 
    -- 'KangarooCourt.Broker.info'
    -- field, and a cloned 
    -- 'KangarooCourt.Broker.channel' field. 
    addEntryUser :: PlayerId 
                 -> BrokerEntry Lobby LobbyMessageI 
                 -> STM BrokerEntry'
    addEntryUser pid be = do
        c <- dupLobbyChan be
        modifyLobbyData be (addLobbyUser pid)
        pure $ BrokerEntry (info be) c
    
    
    runBrokerCommand pid broker b = do 
        putStrLn $ "Running Broker Command: " ++ show b
        inner b
      where
        inner (CreateLobby id l) =
            tryCreateLobby pid id l broker 
        inner (JoinLobby id) = 
            tryJoinLobby pid id broker
        inner ListLobbies =
            listLobbies broker

    -- | Obtain the proper response for a user request
    -- to try and create a lobby.
    tryCreateLobby :: PlayerId -- ^ Player creating this lobby
                   -> Text -- ^ ID for this lobby
                   -> Lobby -- ^ Initial information for the lobby
                   -> GameBroker -- ^ Broker to add the lobby in
                   -> IO (BrokerResponseI) -- ^ Response to send to the user
    tryCreateLobby pid lid info broker = do
        c <- atomically $ do
            i <- createLobbyAt lid info broker
            traverse (addEntryUser pid) i
        pure $ maybe (NameTaken lid) (JoinedLobby lid) c

    -- | Obtain the proper response for a user request
    -- to list the lobbies.
    listLobbies :: GameBroker -- ^ Broker to list the lobbies of
                -> IO (BrokerResponseI) -- ^ Response to the request
    listLobbies = fmap GetLobbyList . peekLobbiesIO

    -- | Obtain a proper response for a user request to
    -- try and join a lobby.
    tryJoinLobby :: PlayerId -- ^ Player trying to join the lobby
                 -> Text -- ^ Lobby ID to join
                 -> GameBroker -- ^ Broker to lookup the loby to join
                 -> IO (BrokerResponseI) -- ^ Result of the attempt
    tryJoinLobby pid key broker = do
        c <- atomically $ do
            l <- getLobbyAt key broker
            traverse (addEntryUser pid) l
        pure $ maybe (NotFound key) (JoinedLobby key) c
    
    recvBrokerCommand :: (MonadRecving m t)
                      => t -> m (Maybe BrokerCommand)
    recvBrokerCommand t = do
        r <- recvMessage t
        pure $ decode r

    sendBrokerResponse :: (MonadSending m t)
                      => t -> BrokerResponseE -> m ()
    sendBrokerResponse = sendJSONMessage

    runBrokerSession :: (MonadSending IO t, MonadRecving IO t)
                     => PlayerId 
                     -> Broker Text Lobby LobbyMessageI
                     -> t
                     -> IO () 
    runBrokerSession pid be t = do
        let loop = runBrokerSession pid be t
        putStrLn $ "Waiting on a broker message..."
        m <- recvBrokerCommand t
        putStrLn $ "Got a broker message: " ++ show m
        r <- case m of
            Nothing -> pure BadMessage
            Just m' -> runBrokerCommand pid be m' 
        case r of
            JoinedLobby id be -> do
                sendBrokerResponse t (JoinedLobby id ())
                runLobbySession be pid t 
            GetLobbyList l -> sendBrokerResponse t (GetLobbyList l) >> loop
            NotFound id -> sendBrokerResponse t (NotFound id) >> loop
            NameTaken id -> sendBrokerResponse t (NameTaken id) >> loop
    
    sendGameMessage :: (MonadSending m t)
                    => t 
                    -> (GameMessage Lobby LobbyMessageE)
                    -> m ()
    sendGameMessage = sendJSONMessage

    runLobbySession be pid t = do
        stid <- forkIO $ runLobbySessionRecv be pid t 
        inner stid
        where
            inner stid = do
                let loop = inner stid
                r <- atomically $ readTChan (channel be)
                case r of
                    InfoUpdated l -> sendGameMessage t (InfoUpdated l) >> loop
                    GameMessage (ChatMessage pid m) -> 
                        sendGameMessage t (GameMessage $ ChatMessage pid m) >> loop
                    GameMessage (GameStarted comm) -> do
                        sendGameMessage t (GameMessage $ GameStarted ())
                        killThread stid
                        startGameCommunication pid t comm

    runLobbySessionRecv be pid t = forever $ do
        putStrLn $ 
            "RunLobbySessionRecv: Trying to read a message..."
        r <- recvJSONMessage t
        traverse_ (runLobbyCommand be pid t) r

    runLobbyCommand :: (MonadRecving IO t)
                    => BrokerEntry'
                    -> PlayerId
                    -> t
                    -> LobbyCommand
                    -> IO ()
    runLobbyCommand be pid t c = do 
        putStrLn $ "Running a lobby command: " ++ show c
        inner c
      where
        inner (AppendDeck deck) = 
            atomically $ modifyLobbyData be 
                $ field @"deck" <>~ deck
        inner (SendChat chat) =
            atomically $ 
                writeTChan (channel be) $ 
                GameMessage $ 
                ChatMessage pid chat

            
    startGameCommunication pid t comm = do
        r <- forkIO recvLoop
        sendLoop r
        where
            sendLoop threadId = do 
                m <- atomically . readTChan . recvC $ comm
                sendJSONMessage t m
                if isGameOver . snd $ m then do
                    killThread threadId
                    pure ()
                else
                    sendLoop threadId
            recvLoop = forever $ do
                m <- recvJSONMessage t
                case m of
                    Nothing -> pure ()
                    Just a -> atomically (writeTQueue (sendQ comm) (pid, a)) >> pure ()