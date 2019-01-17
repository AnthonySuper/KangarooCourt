{-# LANGUAGE DeriveGeneric #-}

module KangarooCourt.BrokerMessage where
    import GHC.Generics
    import Data.Aeson

    data GameMessage l c
        = InfoUpdated l
        | GameMessage c
        deriving (Show, Read, Eq, Ord, Generic)


    instance (ToJSON l, ToJSON c) =>
        ToJSON (GameMessage l c)

    instance (FromJSON l, FromJSON c) =>
        FromJSON (GameMessage l c)
