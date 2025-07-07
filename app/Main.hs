{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (Value(Number))
import Control.Exception (bracket)
import Control.Monad (replicateM_)

import Gym.Environment
import Gym.Core

main :: IO ()
main = do
  putStrLn "Creating CartPole-v1 environment..."
  result <- makeEnv "CartPole-v1"
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right env -> bracket (return env) closeEnv $ \env -> do
      putStrLn "Environment created successfully!"
      
      putStrLn "Resetting environment..."
      resetResult <- reset env
      case resetResult of
        Left err -> putStrLn $ "Reset error: " ++ show err
        Right (Observation obs) -> do
          putStrLn $ "Initial observation: " ++ show obs
          
          putStrLn "Running 5 random steps..."
          runSteps env 5
          
          putStrLn "Done!"

runSteps :: Environment -> Int -> IO ()
runSteps env 0 = return ()
runSteps env n = do
  stepResult <- step env (Action (Number 0))
  case stepResult of
    Left err -> putStrLn $ "Step error: " ++ show err
    Right result -> do
      putStrLn $ "Step " ++ show (6 - n) ++ ": reward=" ++ show (stepReward result) ++ 
                 ", terminated=" ++ show (stepTerminated result)
      if stepTerminated result || stepTruncated result
        then do
          putStrLn "Episode ended, resetting..."
          resetResult <- reset env
          case resetResult of
            Left err -> putStrLn $ "Reset error: " ++ show err
            Right _ -> runSteps env (n - 1)
        else runSteps env (n - 1)