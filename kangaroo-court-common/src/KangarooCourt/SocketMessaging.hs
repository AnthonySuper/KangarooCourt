{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module KangarooCourt.SocketMessaging where
    
    import Network.Socket (Socket)
    import Network.Socket.ByteString.Lazy
    import GHC.Generics
    import Data.ByteString.Lazy (ByteString)
    import qualified Data.ByteString.Lazy as ByteString
    import Data.Binary
    import GHC.Int (Int64)
    import KangarooCourt.Messaging
    import Control.Monad
    import System.Environment
    import Data.List

    newtype Messenger
        = Messenger 
        { getMessenger :: Socket }
        deriving (Show, Eq, Generic)

    sendLength :: Messenger -> ByteString -> IO ()
    sendLength s m =
        sendAll (getMessenger s) $ 
            encode (ByteString.length m)

    logMessage :: String -> IO ()
    logMessage msg = do
        e <- getProgName
        let msg' = e ++ ": " ++ msg
        if isInfixOf "app" e then
            appendFile "log.txt" msg'
        else 
            putStrLn msg'
           

    metaLength :: Int64
    metaLength = ByteString.length . encode . ByteString.length $ mempty

    sendMessengerMessage :: Messenger -> ByteString -> IO ()
    sendMessengerMessage s m = do
        logMessage $
            "Trying to send " ++ show m
                ++ " to " ++ show (getMessenger s)
        sendLength s m
        sendAll (getMessenger s) m

    recvAll :: Messenger -> Int64 -> IO ByteString
    recvAll m l = do
        logMessage $
            "RecvAll: trying to read " ++ 
            show l ++ " bytes from " ++ show (getMessenger m)
        m <- chunkRecv m l
        logMessage $
            "RecvAll: Successfully read bytes " ++
            show m
        pure m 

    chunkRecv :: Messenger -> Int64 -> IO ByteString
    chunkRecv _ 0 = pure mempty
    chunkRecv m@(Messenger s) i = do
        let clampSize = min i 4026
        logMessage $ 
            "ChunkRecv: attempting chunk of size " ++ 
            show i ++ 
            "(clamped to " ++ 
            show clampSize ++
            ")"
        b <- recv s (min i 4026)
        when (ByteString.length b == 0) $ do
            logMessage "ChunkRecv: Socket is closed, erroring!"
            fail "Socket is closed!!"
        logMessage $ 
            "ChunkRecv: actually read " ++
            show (ByteString.length b) ++ " bytes : " ++ show b
        b' <- chunkRecv m (i - ByteString.length b)
        pure $ b <> b'

    recvMessengerMessage :: Messenger -> IO ByteString
    recvMessengerMessage m = do
        logMessage $ 
            "Attempting to read length portion for a message ("
            ++ show metaLength ++ " bytes in size)"
        len <- recvAll m metaLength
        let len' = decode len
        logMessage $
            "Read length of " ++ show len' ++ ", trying that..."
        recvAll m len'
        
    instance MonadSending IO Messenger where
        sendMessage = sendMessengerMessage

    instance MonadRecving IO Messenger where
        recvMessage = recvMessengerMessage

    