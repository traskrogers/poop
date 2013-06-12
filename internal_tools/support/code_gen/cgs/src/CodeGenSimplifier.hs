module CodeGenSimplifier (
  main
) where

import CodeGenSimplifier.Help (help)
import CodeGenSimplifier.Options (Options(..), parseArgs, fixOpts)
import Simplifier (simplify)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

--------------------------------------------------------------------------------

main :: IO ()
main = runCodeGenSimplifier

runCodeGenSimplifier :: IO ()
runCodeGenSimplifier = do
  args <- getArgs
  (files, opts) <- if any (`elem` args) ["-h", "--help"] || null args
    then help >> exitSuccess
    else maybe (error "Bad args") return $ parseArgs args
  if null files
    then putStrLn "No input files." >> exitFailure
    else return ()
  opts' <- fixOpts opts
  putStrLn $ "Output dir: " ++ oOutDir opts'
  putStrLn $ "Externalize output dir: " ++ oExternalizeDir opts'
  simplify opts' files


