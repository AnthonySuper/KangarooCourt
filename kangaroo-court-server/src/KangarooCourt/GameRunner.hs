{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
module KangarooCourt.GameRunner where

    import Control.Monad.State.Class
    import KangarooCourt.ServerState
    import KangarooCourt.Command
    import KangarooCourt.Card
    import Control.Monad.Error.Class
    import GHC.Generics
    import Control.Lens
    import Data.Generics.Product
    import Data.Generics.Sum
    import Data.Map.Strict (Map)
    import Data.Text (Text)

    type Checker m =
        ( MonadState ServerState m
        , MonadError () m)


    ensure err predicate = do
        s <- get
        if predicate s then
            throwError err
        else pure ()

    ensure' :: (MonadState s m, MonadError () m)
            => (s -> Bool)
            -> m () 
    ensure' = ensure ()

    -- | Commands for the server: sometimes
    -- the server must be the one to send an event!
    data ServerCommand
        = DoNothing
        -- ^ No further action must be taken
        | WantRoutineCard
        -- ^ We want a job card
        | EndGame (Map PlayerId Int)
        -- ^ The game has ended with the given score map
        deriving (Show, Read, Eq, Ord, Generic)
    
    noCommand e = (DoNothing, e)
    noCommand' e = pure (DoNothing, e)

    callButWait' :: (Checker m) 
                 => PlayerId 
                 -> Explanation 
                 -> m (ServerCommand, GameEvent)
    callButWait' pid exp = do
        ensureButWaitable pid 
        modify $ callButWait pid
        noCommand' (CallButWait exp)

    acceptRoutine' :: (Checker m)
                   => PlayerId
                   -> m (ServerCommand, GameEvent)
    acceptRoutine' pid = do
        ensureButWaitable pid
        modify $ acceptRoutine pid
        noCommand' AcceptRoutine
    
    validateButWait' :: (Checker m)
                     => PlayerId
                     -> m (ServerCommand, GameEvent)
    validateButWait' pid = do
        ensureButWaitVote pid
        modify $ validateButWait pid
        noCommand' ValidateButWait

    rejectButWait' :: (Checker m)
                   => PlayerId
                   -> m (ServerCommand, GameEvent)
    rejectButWait' pid = do
        ensureButWaitVote pid
        modify $ rejectButWait pid
        noCommand' ValidateButWait

    performRoutine' :: (Checker m)
                    => PlayerId
                    -> Explanation
                    -> m (ServerCommand, GameEvent)
    performRoutine' pid e = do
        ensure' $ isTurn pid
        ensure' isWaitingPerform
        modify performRoutine
        pure (WantRoutineCard, PerformRoutine e)

    guessAnimal' :: (Checker m)
                 => PlayerId
                 -> AnimalCard
                 -> m (ServerCommand, GameEvent)
    guessAnimal' pid ac = do
        ensure' $ isTurn pid
        ensure' isWaitingPerform
        s <- get
        case guessAnimal ac pid s of
            Left e -> withEndGame e ac
            Right s -> do
                put s
                noCommand' $ GuessAnimal ac

    sendChat' :: (Checker m)
              => PlayerId
              -> Text
              -> m (ServerCommand, GameEvent)
    sendChat' pid t =
        noCommand' $ SendGameChat t

    withEndGame e ac = pure (EndGame map, GuessAnimal ac)
        where
            map = getField @"playerScore" <$> e
                
    -- | Make sure that we're in a state where this player
    -- either vote to "But wait!" or decide not to
    ensureButWaitable :: (Checker m)
                      => PlayerId
                      -> m ()
    ensureButWaitable pid = do
        ensure' isInWaitForButWaits
        ensure' $ notDecidedButWait pid
        ensure' $ isNotTurn pid

    -- | Make sure we're in a state where this player can
    -- vote on if a "but wait!" is valid or not
    ensureButWaitVote :: (Checker m)
                      => PlayerId
                      -> m ()
    ensureButWaitVote pid = do
        ensure' isEvaluatingButWait
        ensure' $ hasNotButWaitVoted pid

    -- | Is our game in the "evaluating a 'but wait!' claim"
    -- state or not?
    isEvaluatingButWait :: ServerState -> Bool
    isEvaluatingButWait
        = has
        $ field @"roundState"
        . field @"roundStage"
        . _Ctor @"EvaluatingButWait"

    -- | Is our game in the state where players are deciding
    -- if they want to call a "But wait!"
    isInWaitForButWaits :: ServerState -> Bool
    isInWaitForButWaits 
        = has
        $ field @"roundState"
        . field @"roundStage"
        . _Ctor @"WaitForButWaits"
    
    -- | Has the given player not yet decided if they are going
    -- to call a "But wait!" or not?
    notDecidedButWait :: PlayerId -> ServerState -> Bool 
    notDecidedButWait pid
        = hasn't
        $ field @"roundState"
        . field @"roundStage"
        . _Ctor @"WaitForButWaits"
        . at pid

    -- | Has the player not yet voted on a "But wait!" or not?
    hasNotButWaitVoted :: PlayerId -> ServerState -> Bool
    hasNotButWaitVoted pid
        = hasn't
        $ field @"roundState"
        . field @"roundStage"
        . _Ctor @"EvaluatingButWait"
        . _2
        . at pid 

    -- | Is it not this player's turn?
    isNotTurn :: PlayerId -> ServerState -> Bool
    isNotTurn pid
        = (/= pid)
        . view
        ( field @"roundState"
        . field @"currentPlayer" )

    -- | Is it this player's turn?
    isTurn :: PlayerId -> ServerState -> Bool
    isTurn pid
        = (== pid)
        . view
        ( field @"roundState"
        . field @"currentPlayer" )


    -- | Is the game in the "wait for a performance" state?
    isWaitingPerform :: ServerState -> Bool
    isWaitingPerform
        = has
        $ field @"roundState"
        . field @"roundStage"
        . _Ctor @"WaitForPerform"

    runStateCmd (Just p) (PerformRoutine e) =
        performRoutine' p e
    runStateCmd (Just p) AcceptRoutine =
        acceptRoutine' p
    runStateCmd (Just p) (CallButWait e) =
        callButWait' p e
    runStateCmd (Just p) ValidateButWait =
        validateButWait' p
    runStateCmd (Just p) RejectButWait =
        rejectButWait' p
    runStateCmd (Just p) (GuessAnimal ac) =
        guessAnimal' p ac
    runStateCmd _ _ = throwError ()