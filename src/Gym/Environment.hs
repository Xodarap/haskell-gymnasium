{-# LANGUAGE OverloadedStrings #-}

-- | High-level interface for interacting with OpenAI Gymnasium environments.
--
-- This module provides the main API for creating, managing, and interacting
-- with Gymnasium environments from Haskell. It handles the communication
-- with the Python subprocess and provides a clean, type-safe interface.
--
-- ==== __Basic Usage__
--
-- @
-- import Gym.Environment
-- import Gym.Core
-- import Data.Aeson (toJSON)
-- 
-- main :: IO ()
-- main = do
--   result <- makeEnv "CartPole-v1"
--   case result of
--     Left err -> print err
--     Right env -> do
--       obs <- reset env
--       case obs of
--         Left err -> print err
--         Right observation -> do
--           stepResult <- step env (Action (toJSON (0 :: Int)))
--           print stepResult
--           closeEnv env
-- @
module Gym.Environment
  ( Environment(..)
  , makeEnv
  , closeEnv
  , reset
  , step
  , render
  ) where

import Data.Aeson (Value(Object, Number, Bool))
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific (toRealFloat)

import Gym.Core
import Gym.Internal.Python

-- | Represents a Gymnasium environment instance.
--
-- This type wraps the Python subprocess handle and environment metadata,
-- providing a safe interface for interacting with the underlying Gymnasium
-- environment.
data Environment = Environment
  { envPython :: PythonEnv  -- ^ Handle to the Python subprocess
  , envName :: Text         -- ^ Name of the Gymnasium environment
  }

-- | Create a new Gymnasium environment.
--
-- This function starts a Python subprocess running the specified Gymnasium
-- environment and returns a handle for interacting with it.
--
-- ==== __Parameters__
--
-- * @name@ - The name of the Gymnasium environment (e.g., \"CartPole-v1\", \"Breakout-v5\")
--
-- ==== __Returns__
--
-- * @Right Environment@ - Successfully created environment
-- * @Left GymError@ - Error occurred during environment creation
--
-- ==== __Examples__
--
-- >>> env <- makeEnv "CartPole-v1"
-- >>> env <- makeEnv "LunarLander-v2"
--
-- __Note:__ Always call 'closeEnv' when done to clean up resources.
makeEnv :: Text -> IO (Either GymError Environment)
makeEnv name = do
  result <- createPythonEnv name
  case result of
    Left err -> return $ Left $ PythonError err
    Right pyEnv -> return $ Right $ Environment pyEnv name

-- | Close and clean up a Gymnasium environment.
--
-- This function terminates the Python subprocess and releases all associated
-- resources. The environment handle becomes invalid after this call.
--
-- ==== __Parameters__
--
-- * @env@ - The environment to close
--
-- __Note:__ This function should always be called when done with an environment
-- to prevent resource leaks.
closeEnv :: Environment -> IO ()
closeEnv env = destroyPythonEnv (envPython env)

-- | Reset the environment to its initial state.
--
-- This function resets the Gymnasium environment and returns the initial
-- observation. This should be called at the beginning of each episode.
--
-- ==== __Parameters__
--
-- * @env@ - The environment to reset
--
-- ==== __Returns__
--
-- * @Right Observation@ - The initial observation after reset
-- * @Left GymError@ - Error occurred during reset
--
-- ==== __Examples__
--
-- >>> result <- reset env
-- >>> case result of
-- >>>   Right obs -> putStrLn "Environment reset successfully"
-- >>>   Left err -> print err
reset :: Environment -> IO (Either GymError Observation)
reset env = do
  result <- resetEnv (envPython env)
  case result of
    Left err -> return $ Left $ PythonError err
    Right value -> case parseResetResponse value of
      Nothing -> return $ Left $ ParseError "Failed to parse reset response"
      Just obs -> return $ Right obs

-- | Take a step in the environment with the given action.
--
-- This function executes an action in the Gymnasium environment and returns
-- the resulting observation, reward, termination flags, and additional info.
--
-- ==== __Parameters__
--
-- * @env@ - The environment to step
-- * @action@ - The action to take
--
-- ==== __Returns__
--
-- * @Right StepResult@ - The result of the step containing observation, reward, etc.
-- * @Left GymError@ - Error occurred during the step
--
-- ==== __Examples__
--
-- >>> import Data.Aeson (toJSON)
-- >>> result <- step env (Action (toJSON (0 :: Int)))  -- Discrete action
-- >>> result <- step env (Action (toJSON [0.5, -0.3])) -- Continuous action
step :: Environment -> Action -> IO (Either GymError StepResult)
step env (Action action) = do
  result <- stepEnv (envPython env) action
  case result of
    Left err -> return $ Left $ PythonError err
    Right value -> case parseStepResponse value of
      Nothing -> return $ Left $ ParseError "Failed to parse step response"
      Just stepResult -> return $ Right stepResult

-- | Render the current state of the environment.
--
-- This function triggers the environment's rendering system, which may
-- display the current state visually (depending on the environment and
-- render mode configuration).
--
-- ==== __Parameters__
--
-- * @env@ - The environment to render
--
-- ==== __Returns__
--
-- * @Right ()@ - Rendering completed successfully
-- * @Left GymError@ - Error occurred during rendering
--
-- __Note:__ The actual rendering behavior depends on the specific Gymnasium
-- environment and its configured render mode.
render :: Environment -> IO (Either GymError ())
render env = do
  result <- renderEnv (envPython env)
  case result of
    Left err -> return $ Left $ PythonError err
    Right () -> return $ Right ()

-- | Parse the JSON response from a reset operation.
--
-- Internal function that extracts the observation from the JSON response
-- returned by the Python subprocess after a reset operation.
parseResetResponse :: Value -> Maybe Observation
parseResetResponse (Object obj) = do
  obs <- KM.lookup "observation" obj
  return $ Observation obs
parseResetResponse _ = Nothing

-- | Parse the JSON response from a step operation.
--
-- Internal function that extracts all step result components (observation,
-- reward, termination flags, and info) from the JSON response returned by
-- the Python subprocess after a step operation.
parseStepResponse :: Value -> Maybe StepResult
parseStepResponse (Object obj) = do
  obs <- KM.lookup "observation" obj
  reward <- KM.lookup "reward" obj >>= parseNumber
  terminated <- KM.lookup "terminated" obj >>= parseBool
  truncated <- KM.lookup "truncated" obj >>= parseBool
  info <- KM.lookup "info" obj
  return $ StepResult (Observation obs) reward terminated truncated info
parseStepResponse _ = Nothing

-- | Parse a JSON number value to a Haskell Double.
--
-- Internal helper function for extracting numeric values from JSON responses.
parseNumber :: Value -> Maybe Double
parseNumber (Number n) = Just $ toRealFloat n
parseNumber _ = Nothing

-- | Parse a JSON boolean value to a Haskell Bool.
--
-- Internal helper function for extracting boolean values from JSON responses.
parseBool :: Value -> Maybe Bool
parseBool (Bool b) = Just b
parseBool _ = Nothing