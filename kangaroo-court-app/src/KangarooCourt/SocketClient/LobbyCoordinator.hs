{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KangarooCourt.SocketClient.LobbyCoordinator (runLobbyCoordinator) where
    
    import KangarooCourt.Messaging
    import KangarooCourt.Lobby
    import KangarooCourt.BrokerMessage
    import KangarooCourt.Deck
    import qualified Data.Map.Strict as Map
    import Data.String (fromString)

    defaultDeck = Deck mempty mempty mempty
    lobbyMaker name = Lobby (fromString name) mempty defaultDeck
    
    runLobbyCoordinator t = do
        let loop = runLobbyCoordinator t
        putStrLn "What would you like to do? (h for help)"
        r <- getLine
        case r of
            "h" -> help >> loop
            ('l' : rest) -> listLobbies t >> loop
            ('c' : ' ' : name) -> tryCreate name t
            ('j' : ' ' : name) -> tryJoin name t
            a -> putStrLn ("Unrecognized command: " ++ show a) >> loop


    recvBrokerMessage :: forall t. (MonadRecving IO t)
                      => t -> IO BrokerResponseE
    recvBrokerMessage t = do
        r <- recvJSONMessage t
        case r of
            Nothing -> do
                putStrLn "Got a bad message, desperately looping"
                recvBrokerMessage t
            Just r' -> pure r'
    
    tryCreate name t = do
        putStrLn $ 
            "Trying to make a lobby named " ++ show name
        sendJSONMessage t $ 
            CreateLobby (fromString name) (lobbyMaker name)
        createJoinResponse t

    tryJoin name t = do
        putStrLn $
            "Trying to join a lobby named " ++ show name
        sendJSONMessage t $
            JoinLobby (fromString name)
        createJoinResponse t

    createJoinResponse t = do
        r <- recvBrokerMessage t
        case r of
            JoinedLobby n _ -> do
                putStrLn $ "Joined a lobby: " ++ show n
                pure ()
            NameTaken n -> do
                putStrLn $ "Error: name " ++ show n ++ " is taken!"
                runLobbyCoordinator t
            NotFound n -> do
                putStrLn $ "Error: name " ++ show n ++ " is not found!"
                runLobbyCoordinator t
            n -> do
                putStrLn $ "Recv'd unexpected message: " ++ show n
                runLobbyCoordinator t
    

    listLobbies t = do
        sendJSONMessage t ListLobbies
        r <- recvBrokerMessage t
        formatLobbyList r
        pure ()

    formatLobbyList (GetLobbyList l) = do
        putStrLn "Got a list"
        mapM_ (uncurry formatMapEntry) $ Map.toList l
    formatLobbyList a = 
            putStrLn $ "Oh dear! Expected a lobby list, got: " ++ show a

    formatMapEntry t l =
        putStrLn $ show t ++ ":" ++ show l
   

    help = do
        putStrLn "l -> List Lobbies"
        putStrLn "c $NAME -> Create a lobby with a name"
        putStrLn "j $ID -> Join a lobby with a given id"