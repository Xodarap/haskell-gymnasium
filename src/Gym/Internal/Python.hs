{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Internal module for managing Python subprocess communication.
--
-- This module handles the low-level details of starting Python subprocesses,
-- communicating with them via JSON over stdin/stdout, and managing process
-- lifecycle. It is not intended to be used directly by end users.
--
-- The module implements a simple protocol where commands are sent as text
-- lines to the Python process and responses are received as JSON strings.
-- The Python subprocess runs a small script that creates a Gymnasium
-- environment and responds to commands.
module Gym.Internal.Python
  ( PythonEnv(..)
  , createPythonEnv
  , destroyPythonEnv
  , stepEnv
  , resetEnv
  , renderEnv
  ) where

import Data.Aeson (Value, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import System.Process
import System.IO
import System.Mem.Weak (Weak, mkWeak, deRefWeak)
import Control.Exception (try, SomeException)

-- | Handle to a Python subprocess running a Gymnasium environment.
--
-- This type encapsulates all the resources needed to communicate with
-- a Python subprocess, including process handles, I/O handles, and
-- automatic cleanup mechanisms.
data PythonEnv = PythonEnv
  { envHandle :: ProcessHandle    -- ^ Handle to the Python process
  , envStdin :: Handle           -- ^ Standard input handle for sending commands
  , envStdout :: Handle          -- ^ Standard output handle for receiving responses
  , envName :: Text              -- ^ Name of the Gymnasium environment
  , envFinalizer :: Weak (IO ()) -- ^ Automatic cleanup finalizer
  }

-- | Create a new Python subprocess running a Gymnasium environment.
--
-- This function starts a Python3 process that creates the specified
-- Gymnasium environment and enters a command loop. The process will
-- respond to text commands sent via stdin and return JSON responses
-- via stdout.
--
-- ==== __Parameters__
--
-- * @envName@ - The name of the Gymnasium environment to create
--
-- ==== __Returns__
--
-- * @Right PythonEnv@ - Successfully created Python environment
-- * @Left String@ - Error message if process creation failed
--
-- __Note:__ The returned 'PythonEnv' includes automatic cleanup via
-- finalizers, but it's still recommended to call 'destroyPythonEnv'
-- explicitly when done.
createPythonEnv :: Text -> IO (Either String PythonEnv)
createPythonEnv envName' = do
  let pythonScript = unlines
        [ "import gymnasium as gym"
        , "import json"
        , "import sys"
        , "import numpy as np"
        , ""
        , "class NumpyEncoder(json.JSONEncoder):"
        , "    def default(self, obj):"
        , "        if isinstance(obj, np.ndarray):"
        , "            return obj.tolist()"
        , "        if isinstance(obj, np.integer):"
        , "            return int(obj)"
        , "        if isinstance(obj, np.floating):"
        , "            return float(obj)"
        , "        return super().default(obj)"
        , ""
        , "env = gym.make('" ++ T.unpack envName' ++ "')"
        , ""
        , "while True:"
        , "    try:"
        , "        line = input()"
        , "        if line == 'QUIT':"
        , "            break"
        , "        elif line == 'RESET':"
        , "            obs, info = env.reset()"
        , "            result = {'observation': obs, 'info': info}"
        , "            print(json.dumps(result, cls=NumpyEncoder))"
        , "        elif line.startswith('STEP:'):"
        , "            action = json.loads(line[5:])"
        , "            obs, reward, terminated, truncated, info = env.step(action)"
        , "            result = {"
        , "                'observation': obs,"
        , "                'reward': reward,"
        , "                'terminated': terminated,"
        , "                'truncated': truncated,"
        , "                'info': info"
        , "            }"
        , "            print(json.dumps(result, cls=NumpyEncoder))"
        , "        elif line == 'RENDER':"
        , "            env.render()"
        , "            print('OK')"
        , "        else:"
        , "            print('ERROR: Unknown command')"
        , "    except Exception as e:"
        , "        print(f'ERROR: {str(e)}')"
        ]
  
  (Just hIn, Just hOut, Just _hErr, ph) <- createProcess (proc "python3" ["-c", pythonScript])
    { std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  
  hSetBuffering hIn LineBuffering
  hSetBuffering hOut LineBuffering
  
  -- Create cleanup action for automatic finalization
  let cleanup = do
        _ <- try @SomeException $ hPutStrLn hIn "QUIT"
        _ <- try @SomeException $ hClose hIn
        _ <- try @SomeException $ hClose hOut
        _ <- try @SomeException $ terminateProcess ph
        return ()
  
  -- Register finalizer that will run when PythonEnv is garbage collected
  finalizer <- mkWeak ph cleanup Nothing
  
  return $ Right $ PythonEnv ph hIn hOut envName' finalizer

-- | Destroy a Python environment and clean up all resources.
--
-- This function sends a QUIT command to the Python subprocess, closes
-- all I/O handles, terminates the process, and deregisters the finalizer.
-- After calling this function, the 'PythonEnv' becomes invalid and should
-- not be used.
--
-- ==== __Parameters__
--
-- * @pythonEnv@ - The Python environment to destroy
--
-- __Note:__ This function is exception-safe and will attempt cleanup
-- even if individual operations fail.
destroyPythonEnv :: PythonEnv -> IO ()
destroyPythonEnv pythonEnv = do
  -- Deregister the finalizer since we're manually cleaning up
  _ <- deRefWeak (envFinalizer pythonEnv)
  
  -- Perform cleanup
  _ <- try @SomeException $ hPutStrLn (envStdin pythonEnv) "QUIT"
  _ <- try @SomeException $ hClose (envStdin pythonEnv)
  _ <- try @SomeException $ hClose (envStdout pythonEnv)
  _ <- try @SomeException $ terminateProcess (envHandle pythonEnv)
  return ()

-- | Send a reset command to the Python environment.
--
-- This function sends a \"RESET\" command to the Python subprocess and
-- waits for the JSON response containing the initial observation and info.
--
-- ==== __Parameters__
--
-- * @pythonEnv@ - The Python environment to reset
--
-- ==== __Returns__
--
-- * @Right Value@ - JSON response from the environment reset
-- * @Left String@ - Error message if reset failed or response parsing failed
resetEnv :: PythonEnv -> IO (Either String Value)
resetEnv pythonEnv = do
  hPutStrLn (envStdin pythonEnv) "RESET"
  response <- hGetLine (envStdout pythonEnv)
  case decode (L8.pack response) of
    Just value -> return $ Right value
    Nothing -> return $ Left $ "Failed to parse reset response: " ++ response

-- | Send a step command to the Python environment.
--
-- This function sends a \"STEP:\" command with the encoded action to the
-- Python subprocess and waits for the JSON response containing the step
-- results (observation, reward, termination flags, and info).
--
-- ==== __Parameters__
--
-- * @pythonEnv@ - The Python environment to step
-- * @action@ - The action to take, encoded as a JSON Value
--
-- ==== __Returns__
--
-- * @Right Value@ - JSON response from the environment step
-- * @Left String@ - Error message if step failed or response parsing failed
stepEnv :: PythonEnv -> Value -> IO (Either String Value)
stepEnv pythonEnv action = do
  hPutStrLn (envStdin pythonEnv) $ "STEP:" ++ L8.unpack (encode action)
  response <- hGetLine (envStdout pythonEnv)
  case decode (L8.pack response) of
    Just value -> return $ Right value
    Nothing -> return $ Left $ "Failed to parse step response: " ++ response

-- | Send a render command to the Python environment.
--
-- This function sends a \"RENDER\" command to the Python subprocess and
-- waits for an acknowledgment. The actual rendering behavior depends on
-- the specific Gymnasium environment and its render mode configuration.
--
-- ==== __Parameters__
--
-- * @pythonEnv@ - The Python environment to render
--
-- ==== __Returns__
--
-- * @Right ()@ - Rendering completed successfully
-- * @Left String@ - Error message if rendering failed
renderEnv :: PythonEnv -> IO (Either String ())
renderEnv pythonEnv = do
  hPutStrLn (envStdin pythonEnv) "RENDER"
  response <- hGetLine (envStdout pythonEnv)
  case response of
    "OK" -> return $ Right ()
    _ -> return $ Left $ "Render failed: " ++ response