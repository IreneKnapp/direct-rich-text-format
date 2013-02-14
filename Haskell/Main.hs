module Main (main) where

import qualified System.Environment as IO
import qualified System.Exit as IO
import qualified Text.RTF as RTF


main :: IO ()
main = do
  arguments <- IO.getArgs
  case arguments of
    [] -> do
      putStrLn $ "Usage: test input.rtf ..."
      IO.exitFailure
    _ -> do
      mapM_ (\filePath -> do
                file <- RTF.readFile filePath
                putStrLn $ show file)
            arguments
      IO.exitSuccess

