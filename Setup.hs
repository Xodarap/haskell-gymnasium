import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import System.Process
import System.Exit
import Control.Exception

main = defaultMainWithHooks simpleUserHooks
  { postInst = installPythonDeps
  }

installPythonDeps :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
installPythonDeps _ _ _ _ = do
  putStrLn "Installing Python dependencies for gym-hs..."
  result <- try $ do
    (exitCode, _, stderr) <- readProcessWithExitCode "python3" ["-m", "pip", "install", "gymnasium[classic_control]", "numpy"] ""
    case exitCode of
      ExitSuccess -> putStrLn "Python dependencies installed successfully"
      ExitFailure code -> do
        putStrLn $ "Warning: Failed to install Python dependencies (exit code " ++ show code ++ ")"
        putStrLn $ "stderr: " ++ stderr
        putStrLn "You may need to manually install: pip install gymnasium[classic_control] numpy"
  case result of
    Left (e :: SomeException) -> do
      putStrLn $ "Warning: Could not install Python dependencies: " ++ show e
      putStrLn "You may need to manually install: pip install gymnasium[classic_control] numpy"
    Right _ -> return ()