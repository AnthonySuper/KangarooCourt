{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module KangarooCourt.GameEval where

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

    newtype MonadGameEval m a 
        = MonadGameEval 
        { evalMG :: StateT ServerState m a 
        } deriving ( Functor
                   , Applicative
                   , Monad
                   , MonadTrans
                   , MonadState ServerState
                   )

    instance (MonadKangarooCourtPlaying m) =>
        (MonadKangarooCourtPlaying (MonadGameEval m)) where
             getGameEvent = lift getGameEvent