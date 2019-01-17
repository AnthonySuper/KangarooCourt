module Main where

    import KangarooCourt.Broker
    import KangarooCourt.LobbyServer
    import KangarooCourt.Messaging
    import KangarooCourt.SocketMessaging
    import Network.Socket
    import qualified Control.Exception as E
    import Control.Monad
    import Control.Concurrent
    import Data.Text as Text
    import Data.Text.Encoding (decodeUtf8)
    import Data.ByteString.Lazy (ByteString, toStrict)
    import KangarooCourt.Command

    main :: IO ()
    main = do
        b <- createBrokerIO
        addr <- resolveSocket "3000"
        E.bracket (openSocket addr) close $ acceptLoop b

    resolveSocket port = do
        let hints = defaultHints {
              addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr

    openSocket addr = do
        sock <- socket 
            (addrFamily addr) 
            (addrSocketType addr)
            (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 10
        return sock
    
    acceptLoop broker sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from: " ++ show peer
        forkFinally (acceptTalk broker conn) (\_ -> close conn)
        
    bsToPid :: ByteString -> PlayerId
    bsToPid = PlayerId . decodeUtf8 . toStrict
    
    acceptTalk broker conn = do
        let m = Messenger conn
        pid <- recvMessage m
        putStrLn $ "Running a session for a player with id " ++ show pid 
        when True $ runBrokerSession (bsToPid pid) broker m 

