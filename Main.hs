{-# LANGUAGE RankNTypes, ScopedTypeVariables, TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Main where

import ReadArgs
import Types
import Util

import Data.List
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Maybe
import Control.Lens
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

calcDerivedStats :: SprintData -> DerivedData
calcDerivedStats sd = DD {
      _manHours = mh
    , _sprintHours = sh
    , _rolledPoints = rp
    , _hourAccuracy = 1 - ha
    , _pointAccuracy = 1 - pa
    , _marginOfError = 1 - (1 - ha) * (1 - pa)
    , _hoursPerPoint = (toFloat sh) / (toFloat (fromMaybe 0 $ sd ^. deliveredPoints))
    , _sprintCount = 0
  }
  where
    mh = calcManHours sd
    sh = mh - (fromMaybe 0 $ sd ^. interruptHours)
    rp = (sd ^. plannedPoints) - (fromMaybe 0 $ sd ^. deliveredPoints)
    ha = 1 - ((toFloat sh) / (toFloat mh))
    pa = 1 - ((toFloat (fromMaybe 0 $ sd ^. deliveredPoints)) / (toFloat (sd ^. plannedPoints)))

calcManHours :: SprintData -> Int
calcManHours sd = (sd ^. people) * ((sd ^. workDays) - (sd ^. vacationDays)) * 8

calcPlanningData :: DerivedData -> SprintData -> PlanningData
calcPlanningData dd sd = PD {
      _hourEstimate = wh
    , _hourEstimateLowerBound = lb
    , _hourEstimateUpperBound = ub
    , _hourOverage = o
    , _recommendedPoints = r
    , _backlogPoints = b
  }
  where
    mh = toFloat $ calcManHours sd
    wh = (toFloat (sd ^. plannedPoints)) * (dd ^. hoursPerPoint)
    e = (dd ^. marginOfError) * wh
    lb = wh - e
    ub = wh + e
    o = max (ub - mh) 0
    r = floor $ (mh - (mh * (dd ^. marginOfError))) / (dd ^. hoursPerPoint)
    b = ceiling ((mh - (min mh lb)) / (dd ^. hoursPerPoint))

takeAverage :: Maybe DerivedData -> [DerivedData] -> DerivedData
takeAverage Nothing = foldl1assoc' ddMma
takeAverage (Just start) = foldlassoc' (start ^. sprintCount) start ddMma

ddMma :: Int -> DerivedData -> DerivedData -> DerivedData
ddMma n a b = DD {
      _manHours = round $ mma n (toFloat $ a ^. manHours) (toFloat $ b ^. manHours)
    , _sprintHours = round $ mma n (toFloat $ a ^. sprintHours) (toFloat $ b ^. sprintHours)
    , _rolledPoints = round $ mma n (toFloat $ a ^. rolledPoints) (toFloat $ b ^. rolledPoints)
    , _hourAccuracy = mma n (a ^. hourAccuracy) (b ^. hourAccuracy)
    , _pointAccuracy = mma n (a ^. pointAccuracy) (b ^. pointAccuracy)
    , _marginOfError = mma n (a ^. marginOfError) (b ^. marginOfError)
    , _hoursPerPoint = mma n (a ^. hoursPerPoint) (b ^. hoursPerPoint)
    , _sprintCount = n
  }

getSprintData :: FilePath -> IO (Either String CF)
getSprintData fp = BS.readFile fp >>= return . eitherDecode'

doAnalysis :: CF -> IO ()
doAnalysis x =
  case (x ^. nextSprint) of
    Nothing -> putStrLn "Done"
    Just n -> BSC.putStrLn . encodePretty $ Analysis {
          _averageSprintStats = d'
        , _plannedSprint = n
        , _planningStats = calcPlanningData d' n
      }
  where
    d = map calcDerivedStats (x ^. sprints)
    d' = takeAverage (x ^. startingData) d

main :: IO ()
main = do
        s <- readArgs
        d <- getSprintData $ fromMaybe "data.json" s
        case d of
          Left x -> putStr "File format error: " >> putStrLn x
          Right x -> doAnalysis x