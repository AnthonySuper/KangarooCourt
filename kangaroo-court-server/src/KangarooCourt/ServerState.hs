{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KangarooCourt.ServerState  where
    import GHC.Generics
    import KangarooCourt.Card
    import KangarooCourt.Command
    import Control.Lens
    import Data.Generics.Product
    import Data.Generics.Sum
    import Data.Ratio ((%))

    import Data.Map.Strict (Map)
    import qualified Data.Map.Strict as Map

    -- | Helper type to model the result of an election
    data ElectionResult
        = Win
        -- ^ The election has been one (over half voted yes)
        | Loss
        -- ^ The election has been lost (over half voted no)
        | Pending
        -- ^ The election is pending (not enough votes to call)
        deriving (Show, Read, Eq, Ord)

    -- | The state the server keeps track of for each individual player
    data PlayerState
        = PlayerState
        { playerAnimal :: Maybe AnimalCard
        -- ^ What animal is assigned to this player?
        , playerJob :: Maybe JobCard
        -- ^ What job is assigned to this player?
        , playerScore :: Int 
        -- ^ What score does this player currently have?
        } deriving (Show, Read, Eq, Ord, Generic)

    -- | A "default" player. Represents a player who is
    -- in some way "new to the game" essentially!
    defaultPlayer = PlayerState Nothing Nothing 0

    -- | Keep track of the "Stage" of a given round.
    -- Each case represents a possible state of the round.
    data RoundStage
        = WaitForDraw
        -- ^ We are waiting for the Routine for this round
        -- to be chosen.
        | WaitForPerform
        -- ^ We are waiting for the Player for this round
        -- to actually perform his Routine.
        | WaitForButWaits (Map PlayerId Bool)
        -- ^ We are waiting to see if anybody will call
        -- a "But wait!" on this round. The map represents
        -- who has voted to not call a "But wait!"
        -- Realistically this could be a set, but that's
        -- a refactoring for another time.
        | EvaluatingButWait PlayerId (Map PlayerId Bool)
        -- ^ Somebody has called "But wait!" and we are 
        -- voting on if their complaint is legitimate. The
        -- map represents the legitimacy votes, and contains
        -- both True ("yea") and False ("nay") votes.
        deriving (Show, Read, Eq, Ord, Generic)

    -- | Model all the state of a given round
    data RoundState
        = RoundState
        { roundStage :: RoundStage
        -- ^ What stage of play this round is in
        , playerTurns :: [PlayerId]
        -- ^ The list of turns for the next round
        , currentPlayer :: PlayerId
        -- ^ Which player will perform the Routine on this round?
        } deriving (Show, Read, Eq, Ord, Generic)

    -- | Model the overall server-side state of a game.
    data ServerState 
        = ServerState
        { serverPlayers :: Map PlayerId PlayerState
        -- ^ The state of each individual players
        , roundState :: RoundState
        -- ^ The state of the current round
        } deriving (Show, Read, Eq, Ord, Generic)

    -- | Lens to the player at a given PID.
    -- If this player does not exist it will be created
    playerAt pid =
        field @"serverPlayers" . at pid . non defaultPlayer

    -- | Get the ids of the players as a list
    playerIdsOf :: ServerState -> [PlayerId]
    playerIdsOf = Map.keys . getField @"serverPlayers"

    -- | Get the number of players in the game
    playerCount = length . playerIdsOf
    
    -- | Get the player ID of the player whose turn it is
    currentTurnPlayer = view $
        field @"roundState" . field @"currentPlayer"

    -- | A lens to the player state of the player whose turn it is
    -- Surprisingly useful!
    playerForCurrentTurn :: Lens' ServerState PlayerState
    playerForCurrentTurn = lens getter setter
        where
            getter s = s ^. playerAt (pid s)
            setter s b = s & playerAt (pid s) .~ b
            pid = currentTurnPlayer

    -- Convenience function to count occurences of a value in a map
    countOccurences :: (Eq b) => b -> Map.Map a b -> Int
    countOccurences m =
        Map.foldl (\a m' -> if m == m' then a + 1 else a) 0 

    
    -- | Set the animal for a player with the given ID
    setPlayerAnimal :: PlayerId -> AnimalCard -> ServerState -> ServerState
    setPlayerAnimal player ac =
        playerAt player . field @"playerAnimal" .~ pure ac

    -- | Set the job for a player with the given ID
    setPlayerJob :: PlayerId -> JobCard -> ServerState -> ServerState
    setPlayerJob player job =
        playerAt player . field @"playerJob" .~ pure job
        
    -- | Mark that a player has performed their turn.
    -- This is an internal helper function only
    performRoutineS :: PlayerId -> RoundStage -> RoundStage
    performRoutineS pid WaitForPerform =
        WaitForButWaits $ Map.singleton pid True
    performRoutineS _ a = a

    -- | Set that the player has chosen to perform their routine.
    -- We then enter the routine-judging state, where players decide
    -- if they wish to "But wait!" the other player.
    performRoutine :: ServerState -> ServerState
    performRoutine s = 
        s & field @"roundState" . field @"roundStage" %~
            performRoutineS (currentTurnPlayer s)

    -- | Mark that the given player has accepted this routine
    -- performance, and proceed to the next turn if needed.
    acceptRoutine :: PlayerId -> ServerState -> ServerState
    acceptRoutine pid =
        roundEndWaitButWaits . (field @"roundState" 
            . field @"roundStage"
            . _Ctor @"WaitForButWaits" 
            . at pid ?~ True)

    -- | Internal helper function to indicate that the given player
    -- has called a "But wait!" on the current routine
    callButWaitR :: PlayerId -> RoundStage -> RoundStage
    callButWaitR pid (WaitForButWaits _) =
        EvaluatingButWait pid $ Map.singleton pid True
    callButWaitR pid r = r

    -- | Mark that the given player has called a "But wait!" on the 
    -- current turn. This transitions the game into the "voting" state.
    -- Note that this does no error handling.
    callButWait :: PlayerId -> ServerState -> ServerState
    callButWait pid = field @"roundState" 
        . field @"roundStage"
        %~ callButWaitR pid

    -- | Internal helper function to change the "But wait!" voting status of
    -- the given player to the provided first argument
    changeButWait :: Bool -> PlayerId -> ServerState -> ServerState
    changeButWait b pid = field @"roundState"
        . field @"roundStage"
        . _Ctor @"EvaluatingButWait"
        . _2
        . at pid ?~ b

    -- | Mark that the given player thinks the "But wait!" 
    -- is legitimate, and move to the next turn if needed
    validateButWait pid = roundEndButWaits . changeButWait True pid

    -- | Mark that the given player thinks the "But wait!"
    -- is illegitimate, and move onto the next round if needed
    rejectButWait pid = roundEndButWaits . changeButWait False pid

    -- | Take a voter total and a map of "Yea" or 
    -- "nay" votes, and find out the state of the election
    -- Basically just a helper function.
    electionResult :: Int -> Map a Bool -> ElectionResult
    electionResult total  m
        | (yesVotes % total) >= (1 % 2) = Win
        | (noVotes % total) >= (1 % 2) = Loss
        | otherwise = Pending
        where
            yesVotes = countOccurences True m
            noVotes = countOccurences False m

    -- | Given the count of total voters, does this round stage
    -- have a verdict on if a "But wait!" has been decided yet?
    hasButWaitsS :: Int -> RoundStage -> ElectionResult
    hasButWaitsS max (WaitForButWaits m) =
        electionResult max m 
    hasButWaitS _ a = Pending

    -- | If the game is deciding on a "But wait!",
    -- this function returns the person who made the challenge
    -- Otherwise, it returns null
    waitCaller :: ServerState -> Maybe PlayerId
    waitCaller = preview $
        field @"roundState" . field @"roundStage" .
        _Ctor @"EvaluatingButWait" . _1

    -- | If the game is deciding on a "But wait!"
    -- this function will apply the modification to the score
    -- of the player who called it.
    -- As you might expect, this is used in the transition away from 
    -- the "But wait!" state, after the election is decided
    modifyButWait :: (Int -> Int) -> ServerState -> ServerState
    modifyButWait cb s = case waitCaller s of
        Nothing -> s
        Just a -> s & playerAt a . field @"playerScore" %~ cb

    -- | Modify the score of the player for the current turn
    modifyTurnScore :: (Int -> Int) -> ServerState -> ServerState
    modifyTurnScore cb = 
        playerForCurrentTurn . field @"playerScore" %~ cb

    -- | Attempt to end the round if the game is in the "But wait!"
    -- state. If the game is not in said state, then this function will
    -- not actually do anything (it acts as the identity)
    roundEndButWaits :: ServerState -> ServerState
    roundEndButWaits s = inner (hasButWaitS (playerCount s) roundStage)
        where
            roundStage = s ^. field @"roundState" . field @"roundStage"
            inner Win = modifyButWait (+ 1) . nextTurn $ s 
            inner Loss = 
                modifyButWait (subtract 1) . 
                modifyTurnScore (+ 1) .
                nextTurn $ s 
            inner Pending = s

    waitButWaitElection = preview $ 
        field @"roundState" . field @"roundStage"
        . _Ctor @"WaitForButWaits"

    -- | Attempt to end the round if the game is in the
    -- "Waiting for a 'but wait'" state---that is,
    -- somebody has attempted their routine, but potentially
    -- not everybody has decided if they want to "But wait!" it.
    -- If everybody *has* decided that they don't want to "But wait!" it,
    -- then this function awards points and moves on to the next turn.
    -- Otherwise, it does nothing (acts as the identity)
    roundEndWaitButWaits :: ServerState -> ServerState
    roundEndWaitButWaits s = case waitButWaitElection s of
        Nothing -> s
        Just a -> if countOccurences True a > playerCount s then
            modifyTurnScore (+ 1) . nextTurn $ s
            else s 

    -- | Given a list with which to replenish the turn list
    -- if it is empty, transition from this turn to the next
    nextTurnR :: [PlayerId] -> RoundState -> RoundState
    nextTurnR (p:r) (RoundState _ [] _) =
        RoundState WaitForDraw r p
        -- If we're out of players in the list, take the head
        -- of the replenishment list, set that as the current,
        -- and set the rest as the pending list
    nextTurnR _ (RoundState _ (x:xs) _) =
        RoundState WaitForDraw xs x
        -- If the pending list still has players, take the head
        -- off of it, and make it the player
    
    -- | A version of nextTurnR that continually loops the 
    -- current turn around: when called, the current player's 
    -- turn becomes the last turn in the pending list.
    nextTurnRLoop :: RoundState -> RoundState
    nextTurnRLoop (RoundState _ (x:xs) c) =
        RoundState WaitForDraw (xs ++ [c]) x
    nextTurnRLoop (RoundState _ [] c) =
        RoundState WaitForDraw [] c

    -- | Move on to the next turn, forcibly without awarding points
    -- or doing other bookkeeping. Use this for testing!
    nextTurn :: ServerState -> ServerState
    nextTurn = field @"roundState" %~ nextTurnRLoop

    -- | Mark an attempt by the player to guess their animal.
    -- If they are correct, this function will return Left with the state
    -- of the players at the end of the game. Otherwise, it will
    -- return Right with the current state of the game, automatically
    -- skipping that player's turn as per the rules.
    guessAnimal :: AnimalCard -> PlayerId -> ServerState -> Either (Map PlayerId PlayerState) ServerState
    guessAnimal a p s
        | hasAnimal a p s = Left $
            s & getField @"serverPlayers" . 
            (playerAt p . field @"playerScore" +~ 3)
        | otherwise = Right $ nextTurn s

    -- | Check if the player has the provided animal
    hasAnimal :: AnimalCard -> PlayerId -> ServerState -> Bool
    hasAnimal ac pid = (pure ac ==) . 
        view (playerAt pid . field @"playerAnimal")