{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Data.Aeson (Value(Number))
import Control.Exception (bracket)

import Gym.Environment
import Gym.Core

main :: IO ()
main = hspec $ do
  describe "Gym Environment" $ do
    it "can create and close CartPole environment" $ do
      result <- makeEnv "CartPole-v1"
      case result of
        Left err -> expectationFailure $ "Failed to create environment: " ++ show err
        Right env -> do
          closeEnv env
          return ()
    
    it "can reset CartPole environment" $ do
      result <- makeEnv "CartPole-v1"
      case result of
        Left err -> expectationFailure $ "Failed to create environment: " ++ show err
        Right gymEnv -> bracket (return gymEnv) closeEnv $ \env -> do
          resetResult <- reset env
          case resetResult of
            Left err -> expectationFailure $ "Failed to reset: " ++ show err
            Right (Observation _) -> return ()
    
    it "can step in CartPole environment" $ do
      result <- makeEnv "CartPole-v1"
      case result of
        Left err -> expectationFailure $ "Failed to create environment: " ++ show err
        Right gymEnv -> bracket (return gymEnv) closeEnv $ \env -> do
          resetResult <- reset env
          case resetResult of
            Left err -> expectationFailure $ "Failed to reset: " ++ show err
            Right _ -> do
              stepResult <- step env (Action (Number 0))
              case stepResult of
                Left err -> expectationFailure $ "Failed to step: " ++ show err
                Right _ -> return ()