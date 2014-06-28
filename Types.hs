{-# LANGUAGE RankNTypes, ScopedTypeVariables, TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Types where

import Data.Aeson
import Data.Aeson.TH
import Control.Lens.TH

data SprintData = SD {
    _people :: Int
  , _workDays :: Int
  , _vacationDays :: Int
  , _interruptHours :: Maybe Int
  , _plannedPoints :: Int
  , _deliveredPoints :: Maybe Int  
} deriving (Show, Eq)

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''SprintData
makeLenses ''SprintData

data DerivedData = DD {
    _manHours :: Int
  , _sprintHours :: Int
  , _rolledPoints :: Int
  , _hourAccuracy :: Float
  , _pointAccuracy :: Float
  , _marginOfError :: Float
  , _hoursPerPoint :: Float
  , _sprintCount :: Int
} deriving (Show,Eq)

makeLenses ''DerivedData
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''DerivedData

data CF = CF {
    _sprints :: [SprintData]
  , _startingData :: Maybe DerivedData
  , _nextSprint :: Maybe SprintData
} deriving (Show)

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''CF
makeLenses ''CF

data PlanningData = PD {
    _hourEstimate :: Float
  , _hourEstimateLowerBound :: Float
  , _hourEstimateUpperBound :: Float
  , _hourOverage :: Float
  , _recommendedPoints :: Int
  , _backlogPoints :: Int
} deriving (Show,Eq)

makeLenses ''PlanningData
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''PlanningData

data Analysis = Analysis {
    _averageSprintStats :: DerivedData
  , _plannedSprint :: SprintData
  , _planningStats :: PlanningData
} deriving (Show)

makeLenses ''Analysis
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Analysis

