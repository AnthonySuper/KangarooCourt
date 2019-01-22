{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module KangarooCourt.SocketClient.InGame where
    import KangarooCourt.ServerState
    import KangarooCourt.Lobby
    import KangarooCourt.Messaging
    import KangarooCourt.Deck
    import KangarooCourt.Command
    import Brick
    import qualified Brick.BChan
    import qualified Graphics.Vty
    import GHC.Generics
    import Data.Generics.Product
    import Data.Generics.Sum
    import Data.Maybe
    import Control.Lens
    import Data.Text (Text)
    import Control.Applicative
    import Data.Map.Strict (Map)
    import qualified Data.Map.Strict as Map

    data AppState 
        = AppState
        { game :: ServerState
        , currentPlayer :: PlayerId
        , routineExplanation :: Maybe Explanation
        , butWaitExplanation :: Maybe Explanation
        , chat :: [(PlayerId, Text)] }
        deriving (Generic)

    data WidgetIdentifier
        = ChatWidget
        | GameWidget
        deriving (Show, Read, Eq, Ord, Enum, Bounded)

    app t 
        = App 
        { appDraw = drawApp
        , appChooseCursor = chooseCursor
        , appHandleEvent = handleEvent t
        , appAttrMap = attrMap'
        , appStartEvent = startEvent
        }

    initState = 
        AppState . setupServerState

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

    startEvent = return

    drawApp :: AppState -> [Widget WidgetIdentifier]
    drawApp as = catMaybes
        [ Just (chatView chat)
        , explainView as
        ]
        where
            chat = getField @"chat" as 


    chatView chat = viewport ChatWidget Vertical $
        vBox . showTail $ reverse . map (uncurry showMessage) $ chat
        where
            showTail [x] = [visible x]
            showTail (x:xs) = x : showTail xs
            showTail [] = []
            showMessage p m =
                txt $ unPlayerId p <> ": " <> m

    
    explainView g = routineVote g <|> butWaitVote g

    routineVote :: AppState 
                -> Maybe (Widget WidgetIdentifier)
    routineVote g
        = showExplainVote
        <$> g ^. field @"routineExplanation"
        <*> pure (g ^. field @"currentPlayer")
        <*> waitButWaits
        <*> pure "Routine Called"
        where
            waitButWaits
                = g
                ^? field @"game"
                . field @"roundState"
                . field @"roundStage"
                . _Ctor @"WaitForButWaits"

    butWaitVote :: AppState
                -> Maybe (Widget WidgetIdentifier)
    butWaitVote g = showButWait
        (orBlank $ getField @"routineExplanation" g)
        (orBlank $ getField @"butWaitExplanation" g)
        (g ^. field @"currentPlayer")
        <$> butWaitVote
        where
            orBlank = fromMaybe (Explanation "")
            butWaitVote
                = g
                ^? field @"game"
                . field @"roundState"
                . field @"roundStage"
                . _Ctor @"WaitForButWaits"
        
    showExplainVote :: Explanation 
                    -> PlayerId
                    -> Map PlayerId Bool
                    -> Text
                    -> Widget WidgetIdentifier
    showExplainVote (Explanation exp) i v l 
        = vBox
        [ txt l
        , txt exp
        , showVoteStatus i v
        ]

    showButWait :: Explanation
                -> Explanation
                -> PlayerId
                -> Map PlayerId Bool
                -> Widget WidgetIdentifier
    showButWait (Explanation r) bw pid v
        = vBox
        [ txt "But wait called!" 
        , txt r
        , showExplainVote bw pid v "But wait:" 
        ]

    showVoteStatus :: PlayerId
                   -> Map PlayerId Bool
                   -> Widget WidgetIdentifier
    showVoteStatus pid m 
        = showVoteCounts m
        <=> str (showYourVote pid m)

    showYourVote pid m
        = case Map.lookup pid m of
            Nothing -> "Not voted yet"
            (Just True) -> "Voted yea"
            (Just False) -> "Voted nay"

    showVoteCounts :: Map PlayerId Bool
                   -> Widget WidgetIdentifier
    showVoteCounts m 
        = str ("Yes:  " <> show yeaVotes)
        <+> str ("No: " <> show nayVotes)
        where
            yeaVotes = countOccurences True m
            nayVotes = countOccurences False m

    
    chooseCursor :: AppState
                 -> [CursorLocation WidgetIdentifier]
                 -> Maybe (CursorLocation WidgetIdentifier)
    chooseCursor _ = listToMaybe

    handleEvent t s (VtyEvent e) =
        handleInputE t s e
    handleEvent _ s (AppEvent e) = continue $ uncurry (handleAppEvent s) e
    handleEvent _ s _ = undefined

    handleInputE = undefined
    
    handleAppEvent :: AppState
                   -> PlayerId
                   -> GameEvent
                   -> AppState
    handleAppEvent s p e = inner p e s
      where
        inner pid (DealAnimalCard ac) = 
            over (field @"game") $ setPlayerAnimal pid ac
        inner pid (DealJobCard jc) = 
            over (field @"game") $ setPlayerJob pid jc
        inner pid (PerformRoutine explanation) 
            = over (field @"game")  performRoutine
            . set (field @"routineExplanation" . _Just) explanation
        inner pid AcceptRoutine
            = over (field @"game") (acceptRoutine pid)
            . clearOnNewTurn
        inner pid (CallButWait exp)
            = over (field @"game") (callButWait pid)
            . set (field @"butWaitExplanation". _Just) exp
        inner pid ValidateButWait
            = over (field @"game") (validateButWait pid)
            . clearOnNewTurn
        inner pid RejectButWait
            = over (field @"game") (rejectButWait pid)
            . clearOnNewTurn
        inner pid (GuessAnimal ac)
            = over (field @"game") nextTurn
            . clearOnNewTurn
        inner pid (SendGameChat text)
            = over (field @"chat") ((pid, text): )
        
    clearOnNewTurn a 
        = if inNewTurn a then 
            resetExplanations a
            else a

    inNewTurn :: AppState -> Bool 
    inNewTurn 
        = liftA2 (||) inWaitForDraw inWaitForPerform
        
    inWaitForDraw :: AppState -> Bool 
    inWaitForDraw
        = hasRoundStage $ _Ctor @"WaitForDraw"

    hasRoundStage s
        = has
        $ field @"game"
        . field @"roundState"
        . field @"roundStage"
        . s

    inWaitForPerform :: AppState -> Bool
    inWaitForPerform
        = hasRoundStage $ _Ctor @"WaitForPerform"
    
    resetExplanations 
        = reset (field @"routineExplanation")
        . reset (field @"butWaitExplanation")
        where
            reset s = s .~ Nothing

    attrMap' = undefined