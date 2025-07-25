{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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

data PythonEnv = PythonEnv
  { envHandle :: ProcessHandle
  , envStdin :: Handle
  , envStdout :: Handle
  , envName :: Text
  , envFinalizer :: Weak (IO ())
  }

createPythonEnv :: Text -> IO (Either String PythonEnv)
createPythonEnv envName = do
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
        , "env = gym.make('" ++ T.unpack envName ++ "')"
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
  
  (Just hIn, Just hOut, Just hErr, ph) <- createProcess (proc "python3" ["-c", pythonScript])
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
  
  return $ Right $ PythonEnv ph hIn hOut envName finalizer

destroyPythonEnv :: PythonEnv -> IO ()
destroyPythonEnv env = do
  -- Deregister the finalizer since we're manually cleaning up
  _ <- deRefWeak (envFinalizer env)
  
  -- Perform cleanup
  _ <- try @SomeException $ hPutStrLn (envStdin env) "QUIT"
  _ <- try @SomeException $ hClose (envStdin env)
  _ <- try @SomeException $ hClose (envStdout env)
  _ <- try @SomeException $ terminateProcess (envHandle env)
  return ()

resetEnv :: PythonEnv -> IO (Either String Value)
resetEnv env = do
  hPutStrLn (envStdin env) "RESET"
  response <- hGetLine (envStdout env)
  case decode (L8.pack response) of
    Just value -> return $ Right value
    Nothing -> return $ Left $ "Failed to parse reset response: " ++ response

stepEnv :: PythonEnv -> Value -> IO (Either String Value)
stepEnv env action = do
  hPutStrLn (envStdin env) $ "STEP:" ++ L8.unpack (encode action)
  response <- hGetLine (envStdout env)
  case decode (L8.pack response) of
    Just value -> return $ Right value
    Nothing -> return $ Left $ "Failed to parse step response: " ++ response

renderEnv :: PythonEnv -> IO (Either String ())
renderEnv env = do
  hPutStrLn (envStdin env) "RENDER"
  response <- hGetLine (envStdout env)
  case response of
    "OK" -> return $ Right ()
    _ -> return $ Left $ "Render failed: " ++ response