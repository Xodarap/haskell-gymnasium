{-# LANGUAGE OverloadedStrings #-}
module Gym.Environment
  ( Environment(..)
  , makeEnv
  , closeEnv
  , reset
  , step
  , render
  ) where

import Control.Exception (bracket)
import Data.Aeson (Value(Object, Number, Bool), (.:))
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific (toRealFloat)

import Gym.Core
import Gym.Internal.Python

data Environment = Environment
  { envPython :: PythonEnv
  , envName :: Text
  }

makeEnv :: Text -> IO (Either GymError Environment)
makeEnv name = do
  result <- createPythonEnv name
  case result of
    Left err -> return $ Left $ PythonError err
    Right pyEnv -> return $ Right $ Environment pyEnv name

closeEnv :: Environment -> IO ()
closeEnv env = destroyPythonEnv (envPython env)

reset :: Environment -> IO (Either GymError Observation)
reset env = do
  result <- resetEnv (envPython env)
  case result of
    Left err -> return $ Left $ PythonError err
    Right value -> case parseResetResponse value of
      Nothing -> return $ Left $ ParseError "Failed to parse reset response"
      Just obs -> return $ Right obs

step :: Environment -> Action -> IO (Either GymError StepResult)
step env (Action action) = do
  result <- stepEnv (envPython env) action
  case result of
    Left err -> return $ Left $ PythonError err
    Right value -> case parseStepResponse value of
      Nothing -> return $ Left $ ParseError "Failed to parse step response"
      Just stepResult -> return $ Right stepResult

render :: Environment -> IO (Either GymError ())
render env = do
  result <- renderEnv (envPython env)
  case result of
    Left err -> return $ Left $ PythonError err
    Right () -> return $ Right ()

parseResetResponse :: Value -> Maybe Observation
parseResetResponse (Object obj) = do
  obs <- KM.lookup "observation" obj
  return $ Observation obs
parseResetResponse _ = Nothing

parseStepResponse :: Value -> Maybe StepResult
parseStepResponse (Object obj) = do
  obs <- KM.lookup "observation" obj
  reward <- KM.lookup "reward" obj >>= parseNumber
  terminated <- KM.lookup "terminated" obj >>= parseBool
  truncated <- KM.lookup "truncated" obj >>= parseBool
  info <- KM.lookup "info" obj
  return $ StepResult (Observation obs) reward terminated truncated info
parseStepResponse _ = Nothing

parseNumber :: Value -> Maybe Double
parseNumber (Number n) = Just $ toRealFloat n
parseNumber _ = Nothing

parseBool :: Value -> Maybe Bool
parseBool (Bool b) = Just b
parseBool _ = Nothing