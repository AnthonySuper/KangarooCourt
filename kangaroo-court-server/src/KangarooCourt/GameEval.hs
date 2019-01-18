{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module KangarooCourt.GameEval where
    import GHC.Generics
    import KangarooCourt.Command
    import KangarooCourt.ServerError
    import KangarooCourt.ServerState
    import Control.Monad.Trans.Except
    import Control.Monad.Trans.State.Strict 
    import Control.Monad.Trans.Class
    import Control.Monad.State.Class
    import Control.Monad.IO.Class
    import Control.Monad.Error.Class
    import Data.Coerce
    import Data.Map (Map)
    import Control.Concurrent.STM.TQueue (TQueue)
    import Control.Concurrent.STM.TChan (TChan)

    newtype GameEvalT m a 
        = GameEvalT
        { evalMG :: StateT ServerState m a 
        } deriving ( Functor
                   , Applicative
                   , Monad
                   , MonadTrans
                   , MonadState ServerState
                   )

    instance (MonadKangarooCourtPlaying m) =>
        (MonadKangarooCourtPlaying (GameEvalT m)) where
             getGameEvent = lift getGameEvent

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