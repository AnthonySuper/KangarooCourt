{-# LANGUAGE FlexibleContexts #-}
module KangarooCourt.SocketClient where
    
    import KangarooCourt.Messaging
    import Data.ByteString.Lazy (ByteString)
    import qualified Data.ByteString.Lazy as ByteString
    import qualified Data.Text as Text
    import qualified Data.Text.Encoding as TextE
    import KangarooCourt.Deck
    import KangarooCourt.Lobby
    import KangarooCourt.SocketClient.LobbyCoordinator
    import KangarooCourt.SocketClient.InLobby



    runClient t = do
        putStrLn "Enter your player ID"
        r <- getLine
        putStrLn "Registering and connecting..."
        sendMessage t $ ByteString.fromStrict . TextE.encodeUtf8 . Text.pack $ r
        putStrLn "Hopefully that worked lmao"
        runLobbyCoordinator t
        runInLobby t


    