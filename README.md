# Haskell Gymnasium

A Haskell library that provides bindings to OpenAI Gymnasium environments, enabling you to call any Gymnasium environment from Haskell. Implemented by spawning a python subprocess and using stdio to communicate with it.

## Quick Start

### Building

```bash
cabal build
```

### Running the Example

```bash
cabal run gym-hs-example
```

This will create a CartPole-v1 environment and run 5 random steps.

### Running Tests

```bash
cabal test
```

## Usage Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson (Value(Number))
import Gym.Environment
import Gym.Core

main :: IO ()
main = do
  result <- makeEnv "CartPole-v1"
  case result of
    Left err -> print err
    Right env -> do
      -- Reset environment
      resetResult <- reset env
      case resetResult of
        Left err -> print err
        Right (Observation obs) -> do
          putStrLn $ "Initial observation: " ++ show obs
          
          -- Take an action (0 = left, 1 = right for CartPole)
          stepResult <- step env (Action (Number 0))
          case stepResult of
            Left err -> print err
            Right result -> do
              putStrLn $ "Reward: " ++ show (stepReward result)
              putStrLn $ "Done: " ++ show (stepTerminated result)
      
      -- Clean up
      closeEnv env
```