{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module KangarooCourt.GameEval where
    import GHC.Generics
    import KangarooCourt.Deck
    import KangarooCourt.Command
    import KangarooCourt.ServerError
    import KangarooCourt.ServerState
    import KangarooCourt.GameCommunicator
    import KangarooCourt.Lobby
    import KangarooCourt.GameRunner
    import Control.Monad.Trans.Except
    import Control.Monad.Trans.State.Strict 
    import Control.Monad.State.Class
    import Control.Monad.IO.Class
    import Control.Monad.Error.Class
    import Control.Lens
    import Control.Lens.Zoom
    import Control.Lens.Internal.Zoom
    import Data.Coerce
    import Data.Map (Map)
    import Control.Concurrent.STM.TQueue (TQueue)
    import Control.Concurrent.STM.TChan (TChan)
    import Data.Generics.Product

    data GameState =
        GameState
        { serverState :: ServerState
        , deck :: Deck
        , communicator :: GameCommunicator
        } deriving (Generic)

    newtype GameEvalT s a 
        = GameEvalT
        { evalMG :: StateT s (ExceptT () IO) a
        } deriving ( Functor
                   , Applicative
                   , Monad
                   , MonadError ()
                   , MonadState s
                   , MonadIO 
                   )

    type instance Zoomed (GameEvalT s)
        = Focusing (ExceptT () IO)

    instance Zoom (GameEvalT s) (GameEvalT t) s t where
        zoom l (GameEvalT m) = GameEvalT (zoom l m)


    runGameEvalT :: GameEvalT s a
                 -> s
                 -> IO (Either () (a, s))
    runGameEvalT m s =
        runExceptT $ runStateT (evalMG m) s


    createRoundState :: Lobby
                     -> RoundState
    createRoundState lobby
        = RoundState WaitForDraw (tail playerList) (head playerList)
        where
            playerList = getField @"players" lobby

    setupServerState :: Lobby
                     -> ServerState
    setupServerState
        = ServerState mempty . createRoundState

    setupGameState lobby
            = GameState
                (setupServerState lobby)
                (getField @"deck" lobby)
                
    
    evalGame gc lobby = 
        runGameEvalT act $ setupGameState lobby gc
        where
            act = dealCards (getField @"players" lobby) >> runGame

    dealCards pid = do
        mapM_ dealAnimalCard pid
        mapM_ dealJobCard pid 

    dealCard :: Lens' Deck [a] -> GameEvalT GameState a
    dealCard lens = 
        zoom (field @"deck") $ do
            d <- use lens
            case d of
                [] -> throwError ()
                (a:as) -> do
                    assign lens as
                    pure a
        

    sendPlayerEvent' pid e = do
        v <- use (field @"communicator")
        sendPlayerEvent v pid e

    getPlayerEvent' :: GameEvalT GameState (PlayerId, GameEvent)
    getPlayerEvent' = 
        use (field @"communicator") >>= getPlayerEvent

    dealAnimalCard :: PlayerId -> GameEvalT GameState ()
    dealAnimalCard pid = do
        ac <- dealCard (field @"animalCards")
        sendPlayerEvent' pid $ DealAnimalCard ac
        field @"serverState" %= setPlayerAnimal pid ac

    dealJobCard :: PlayerId -> GameEvalT GameState ()
    dealJobCard pid = do
        jc <- dealCard (field @"jobCards")
        sendPlayerEvent' pid $ DealJobCard jc
        field @"serverState" %= setPlayerJob pid jc
        
    runGame :: GameEvalT GameState ()
    runGame = do
        (pid, evt) <- getPlayerEvent'
        catchError (runPlayerEvent pid evt) (const runGame)

    runPlayerEvent pid evt = do
        (cmd, evt) <- zoom (field @"serverState") $ 
            runPlayerEventInner pid evt
        sendPlayerEvent' pid evt
        runServerCommand cmd

    runPlayerEventInner pid = inner
      where
        inner (DealAnimalCard _) = throwError ()
        inner (DealJobCard _) = throwError ()
        inner (DealRoutineCard _) = throwError ()
        inner AcceptRoutine = acceptRoutine' pid
        inner (CallButWait e) = callButWait' pid e
        inner ValidateButWait = validateButWait' pid
        inner RejectButWait = rejectButWait' pid
        inner (GuessAnimal a) = guessAnimal' pid a 
        inner GameOver = throwError ()
        inner (SendGameChat msg) = sendChat' pid msg 


    dealRoutineCard' = do
        jc <- dealCard (field @"routineCards")
        sendPlayerEvent' (PlayerId "") $ DealRoutineCard jc
        field @"serverState" %= dealRoutineCard
    
    runServerCommand :: ServerCommand -> GameEvalT GameState ()
    runServerCommand DoNothing = runGame
    runServerCommand WantRoutineCard = dealRoutineCard' >> runGame
    runServerCommand (EndGame _) = return ()

