{-# LANGUAGE DeriveGeneric #-}
module KangarooCourt.Broker where
    import GHC.Generics
    import Data.Aeson
    import Control.Concurrent.STM.TChan
    import Control.Concurrent.STM.TVar
    import Control.Monad.STM
    import Data.Map.Strict (Map)
    import qualified Data.Map.Strict as Map
    import KangarooCourt.BrokerMessage



    -- | A broker entry keeps track of everything our broker needs
    -- to know about.
    data BrokerEntry l c  
        = BrokerEntry 
        { info :: TVar l
        -- ^ The current state of the information of the broker 
        , channel :: TChan (GameMessage l c)
        -- ^ A chanel that will notify of all updates.
        -- It can also be used for game-specific communication!
        }

    -- | A broker is a manager of many lobbies
    newtype Broker k l c 
        = Broker { getBroker :: TVar (Map k (BrokerEntry l c)) }

    createBroker :: (Ord k) => STM (Broker k l c)
    createBroker = Broker <$> newTVar mempty

    createBrokerIO :: (Ord k) => IO (Broker k l c)
    createBrokerIO = Broker <$> newTVarIO mempty

    addLobby :: (Ord k) => k -> l -> Broker k l c -> STM (BrokerEntry l c)
    addLobby k l b = do
        be <- BrokerEntry <$> newTVar l <*> newBroadcastTChan
        modifyTVar (getBroker b) $ Map.insert k be
        pure be 

    -- | Look at the map of lobbies in a non-transactional way.
    -- This will read much faster than any possible transactional implementation,
    -- but it is technically possible for the values of lobbies to change out from under you
    -- as a result. But that's not really a problem for the basic use-case.
    peekLobbiesIO :: (Ord k) 
                  => Broker k l c 
                  -> IO (Map k l)
    peekLobbiesIO broker = do
        r <- readTVarIO $ getBroker broker
        mapM (readTVarIO . info) r

    createLobbyAt :: (Ord k)
                  => k
                  -> l
                  -> Broker k l c
                  -> STM (Maybe (BrokerEntry l c))
    createLobbyAt key lobby broker = do
        c <- getLobbyAt key broker
        case c of
            Just _ -> pure Nothing
            Nothing -> Just <$> addLobby key lobby broker
    
    getLobbyAt :: (Ord k)
               => k -> Broker k l x -> STM (Maybe (BrokerEntry l x))
    getLobbyAt key broker = do
        b <- readTVar . getBroker $ broker
        pure $ Map.lookup key b

    modifyLobbyData :: BrokerEntry l x -> (l -> l) -> STM ()
    modifyLobbyData be f = do
        e <- readTVar (info be)
        let up = f e
        writeTVar (info be) up
        writeTChan (channel be) $ InfoUpdated up

    getLobbyData :: BrokerEntry l x -> STM l
    getLobbyData = readTVar . info 

    -- | Modify a lobby at the given value, and update all players
    -- with the new value of the lobby!
    modifyLobbyAt :: (Ord k)
                  => k -> Broker k l x -> (l -> l) -> STM ()
    modifyLobbyAt k b f = do
        broke <- getLobbyAt k b
        case broke of
            Nothing -> pure ()
            Just v' -> modifyLobbyData v' f

    dupLobbyChan :: BrokerEntry l x -> STM (TChan (GameMessage l x))
    dupLobbyChan = dupTChan . channel

    dupLobbyChanAt :: (Ord k)
                   => k -> Broker k l x -> STM (Maybe (TChan (GameMessage l x)))
    dupLobbyChanAt key broker = do
        c <- getLobbyAt key broker
        case c of
            Nothing -> pure Nothing
            Just c' -> do
                chan <- dupTChan $ channel c'
                pure $ Just chan

    -- | Remove the lobby at the given key from the map.
    removeLobbyAt :: (Ord k)
                  => k -> Broker k l x -> STM ()
    removeLobbyAt =
        flip (modifyTVar . getBroker) . Map.delete

    mapGameMessage :: (c -> b) -> GameMessage l c -> GameMessage l b
    mapGameMessage f (GameMessage c) = GameMessage $ f c
    mapGameMessage _ (InfoUpdated l) = InfoUpdated l