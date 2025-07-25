{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Core data types for the gym-hs library.
--
-- This module defines the fundamental types used throughout the library
-- for representing actions, observations, step results, and errors when
-- interacting with OpenAI Gymnasium environments.
module Gym.Core
  ( Action(..)
  , Observation(..)
  , StepResult(..)
  , GymError(..)
  ) where

import Data.Aeson (Value, FromJSON, ToJSON)

-- | Represents an action that can be taken in a Gymnasium environment.
--
-- Actions are wrapped JSON values that can represent any valid Gymnasium
-- action space, including discrete actions (integers), continuous actions
-- (arrays of floats), or more complex structured actions.
--
-- ==== __Examples__
--
-- >>> import Data.Aeson (toJSON)
-- >>> Action (toJSON (0 :: Int))  -- Discrete action
-- Action (Number 0.0)
--
-- >>> Action (toJSON [0.5, -0.3])  -- Continuous action
-- Action (Array [Number 0.5,Number (-0.3)])
newtype Action = Action Value
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Represents an observation from a Gymnasium environment.
--
-- Observations are wrapped JSON values that can represent any valid
-- Gymnasium observation space, including images (as nested arrays),
-- sensor readings (as arrays or objects), or discrete states (as integers).
--
-- ==== __Examples__
--
-- >>> import Data.Aeson (toJSON)
-- >>> Observation (toJSON [1.0, 2.0, 3.0])  -- Vector observation
-- Observation (Array [Number 1.0,Number 2.0,Number 3.0])
newtype Observation = Observation Value
  deriving (Show, Eq, FromJSON, ToJSON)

-- | The result of taking a step in a Gymnasium environment.
--
-- Contains all the information returned by the environment after an action
-- is taken, following the standard Gymnasium interface.
data StepResult = StepResult
  { stepObservation :: Observation  -- ^ The new observation after the action
  , stepReward :: Double            -- ^ The reward received for this step
  , stepTerminated :: Bool          -- ^ Whether the episode has ended naturally
  , stepTruncated :: Bool           -- ^ Whether the episode was cut short (e.g., time limit)
  , stepInfo :: Value               -- ^ Additional environment-specific information
  } deriving (Show, Eq)

-- | Errors that can occur when interacting with Gymnasium environments.
data GymError
  = PythonError String      -- ^ Error from the Python subprocess
  | ParseError String       -- ^ Error parsing JSON responses
  | EnvironmentError String -- ^ Environment-specific error
  deriving (Show, Eq)