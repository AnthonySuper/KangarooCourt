{-# LANGUAGE MultiParamTypeClasses #-}
module KangarooCourt.Messaging where

    import Data.ByteString.Lazy (ByteString)
    import Data.Aeson

    class (Monad m) => MonadSending m t where
        sendMessage :: t -> ByteString -> m ()
        -- ^ Send a message given a messaging token

    class (Monad m) => MonadRecving m t where
        recvMessage :: t -> m ByteString
        -- ^ Get a message given a messaging token

    sendJSONMessage t a =
        sendMessage t (encode a)

    recvJSONMessage t = do
        m <- recvMessage t
        pure $ decode m
    