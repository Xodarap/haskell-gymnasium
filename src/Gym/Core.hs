{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Gym.Core
  ( Action(..)
  , Observation(..)
  , StepResult(..)
  , GymError(..)
  ) where

import Data.Aeson (Value, FromJSON, ToJSON)

newtype Action = Action Value
  deriving (Show, Eq, FromJSON, ToJSON)

newtype Observation = Observation Value
  deriving (Show, Eq, FromJSON, ToJSON)

data StepResult = StepResult
  { stepObservation :: Observation
  , stepReward :: Double
  , stepTerminated :: Bool
  , stepTruncated :: Bool
  , stepInfo :: Value
  } deriving (Show, Eq)

data GymError
  = PythonError String
  | ParseError String
  | EnvironmentError String
  deriving (Show, Eq)