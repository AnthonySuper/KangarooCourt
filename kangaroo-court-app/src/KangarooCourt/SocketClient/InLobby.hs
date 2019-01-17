{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module KangarooCourt.SocketClient.InLobby (runInLobby) where
    import KangarooCourt.Messaging
    import KangarooCourt.Lobby
    import KangarooCourt.BrokerMessage
    import KangarooCourt.Deck
    import Control.Concurrent.MVar
    import Control.Concurrent
    import Control.Monad
    import Brick
    import qualified Brick.BChan
    import qualified Graphics.Vty
    import Data.Text (Text)
    import KangarooCourt.Command
    import qualified Brick.Widgets.Edit as BE
    import qualified Graphics.Vty.Input.Events as IE
    import Control.Lens.Operators
    import GHC.Generics
    import Data.Generics.Product
    import Control.Monad.IO.Class
    import Brick.Widgets.Border
    import System.Exit (exitSuccess)

    data AppState
        = AppState
        { lobby :: Lobby
        , chatMessages :: [(PlayerId, Text)]
        , terminalContent :: BE.Editor Text Text }
        deriving (Generic)


    app t = App { appDraw = drawApp
              , appChooseCursor = chooseCursor
              , appHandleEvent = handleEvent t
              , appStartEvent = startEvent
              , appAttrMap = attrMap' }

    initState =
        AppState
            (Lobby "" mempty mempty)
            mempty
            blankEditor

    blankEditor = BE.editorText "textEditor" Nothing ""
    type CustomEvent = GameMessage Lobby LobbyMessageE


    drawApp :: AppState -> [Widget Text]
    drawApp a = pure $
        ( drawLobby (getField @"lobby" a) <+> vBorder <+>
          drawChat (getField @"chatMessages" a)) <=>
        hBorder <=>
        drawEditor (getField @"terminalContent" a)

    drawLobby l = padAll 1
         $  (txt "Name: " <+> txt (getField @"name" l))
        <=> (txt "Players: " <+> showPlayers l)

    showPlayers = vBox .
        map (txt . unPlayerId) .
        getField @"players"

    drawEditor = padAll 1 . 
        BE.renderEditor (hBox . fmap txt) True

    drawChat w = viewport "chatBox" Vertical $
        vBox . showTail $ reverse . map (uncurry showMessage) $ w
        where
            showTail (x:[]) = visible x : []
            showTail (x:xs) = x : showTail xs
            showTail [] = []
            showMessage p m =
                txt $ unPlayerId p <> ": " <> m
            
    chooseCursor :: AppState 
                    -> [CursorLocation Text]
                    -> Maybe (CursorLocation Text)
    chooseCursor _ = showCursorNamed "textEditor"

    startEvent :: AppState -> EventM Text AppState
    startEvent = return

    handleEvent :: (MonadSending IO t)
                => t
                -> AppState 
                -> BrickEvent Text CustomEvent
                -> EventM Text (Next AppState)
    handleEvent t s (VtyEvent e) = 
        handleInputE t s e
    handleEvent _ s (AppEvent e) = handleAppEvent s e
    handleEvent _ s _ = continue s

    handleInputE :: (MonadSending IO t)
                 => t
                 -> AppState 
                 -> IE.Event
                 -> EventM Text (Next AppState)
    handleInputE t s (IE.EvKey IE.KEnter _) = do
        let ec = BE.getEditContents (s ^. field @"terminalContent")
        liftIO $ handleEnter t ec
        continue $ s & field @"terminalContent" .~ blankEditor
    handleInputE _ s e =
        continue =<< (s & field @"terminalContent" %%~ BE.handleEditorEvent e)

    handleAppEvent :: AppState
                   -> CustomEvent
                   -> EventM Text (Next AppState)
    handleAppEvent as (InfoUpdated l) =
        continue $ as & field @"lobby" .~ l
    handleAppEvent as (GameMessage g) =
        handleGameMessage as g

    handleGameMessage as (GameStarted _) = halt as
    handleGameMessage as (ChatMessage pid t) =
        continue $ as & field @"chatMessages" %~ ((pid, t) :)

    handleEnter :: (MonadSending IO t)
                => t -> [Text] -> IO ()
    handleEnter t = handleEnter' t . mconcat

    handleEnter' :: (MonadSending IO t)
                 => t -> Text -> IO ()
    handleEnter' t "/sg" = sendJSONMessage t StartGame
    handleEnter' t ":q" = exitSuccess
    handleEnter' t c = do
        let m = SendChat c
        appendFile "log.txt" $ show m
        sendJSONMessage t m

    attrMap' :: AppState -> AttrMap
    attrMap' = const $ attrMap mempty mempty

    runInLobby t = do
        ec <- Brick.BChan.newBChan 10
        forkIO $ msgReader t ec
        fs <- customMain
            (Graphics.Vty.mkVty Graphics.Vty.defaultConfig)
            (Just ec) 
            (app t)
            initState
        pure ()

    
    readLobbyMsg :: (MonadRecving IO t)
                 => t -> IO (Maybe (GameMessage Lobby LobbyMessageE))
    readLobbyMsg = recvJSONMessage

    readLobbyMsg' t = do
        c <- recvJSONMessage t
        case c of
            Nothing -> readLobbyMsg' t
            Just m' -> pure m'

    msgReader t chan = do
        r <- readLobbyMsg' t
        Brick.BChan.writeBChan chan r
        unless (isGameStart r) $ msgReader t chan

    isGameStart (GameMessage (GameStarted _)) = True
    isGameStart _ = False