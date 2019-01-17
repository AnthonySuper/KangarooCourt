module Main where

    import KangarooCourt.SocketClient
    import qualified Control.Exception as E
    import Network.Socket
    import KangarooCourt.SocketMessaging 



    main :: IO ()
    main = do
        putStrLn "Enter connection address"
        s <- getLine
        putStrLn "Enter connection port"
        p <- getLine
        putStrLn $
            "Trying to connect to " ++ s ++ ":" ++ p
        addr <- resolve s p
        E.bracket (open addr) close talk


    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo 
            (Just hints) (Just host) (Just port)
        return addr

    open addr = do
        socket <- socket 
            (addrFamily addr)
            (addrSocketType addr)
            (addrProtocol addr)
        connect socket $ addrAddress addr
        return socket

    talk = runClient . Messenger
