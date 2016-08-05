{-# LANGUAGE TemplateHaskell #-}
module BT.Types where


import           BT.Util
import           ClassyPrelude
import           Data.Aeson.TH
import           Data.Aeson.Types



data GenConf = MkGenConf
    { lang      :: Text
    , numLevels :: Int
    , numGraphs :: Int
    , seed      :: Maybe Int
    , prctFuns  :: Maybe Float
    , prctMaps  :: Maybe Float
    , prctIf    :: Maybe Float
    } deriving (Show, Eq, Ord)


deriveJSON gconfOptions ''GenConf


data MeasuredGraph = MeasuredGraph
    { nr      :: Maybe Int
    , levels  :: Int
    , rounds  :: Int
    , fetches :: Int
    , genConf :: Maybe GenConf
    } deriving (Show, Eq, Ord)


type MeasuredGraphs = [MeasuredGraph]

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''MeasuredGraph
