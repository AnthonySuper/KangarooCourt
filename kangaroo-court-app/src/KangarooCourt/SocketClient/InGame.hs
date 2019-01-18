{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
    import Data.Maybe
    import Control.Lens
    import Data.Text (Text)


    data AppState 
        = AppState
        { game :: ServerState
        , explanation :: Maybe Explanation
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

    drawApp :: AppState -> [Widget WidgetIdentifier]
    drawApp = const mempty
    
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
            . set (field @"explanation" . _Just) explanation
        inner pid AcceptRoutine
            = over (field @"game") $ acceptRoutine pid
        inner pid (CallButWait exp)
            = over (field @"game") (callButWait pid)
            . set (field @"explanation". _Just) exp
        inner pid ValidateButWait
            = over (field @"game") $ validateButWait pid
        inner pid RejectButWait
            = over (field @"game") $ rejectButWait pid
        inner pid (GuessAnimal ac)
            = over (field @"game") $ nextTurn
        inner pid (SendGameChat text)
            = over (field @"chat") $ ((pid, text): )
        
    attrMap' = undefined