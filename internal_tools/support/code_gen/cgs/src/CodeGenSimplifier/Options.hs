module CodeGenSimplifier.Options (
  Options(..)
, baseOptions
, parseArgs
, fixOpts
) where

import Data.Char (isNumber)
import Data.List (sortBy, isPrefixOf)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory (getCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, (</>))

--------------------------------------------------------------------------------

data Options = Options {
    oOutDir :: FilePath
  , oExternalizeDir :: FilePath
  , oExternalizeData :: Bool
  , oMergeSegs :: Bool
  , oFlattenSegs :: Bool
  , oRemoveNopOpenClose :: Bool
  , oAnonPrefix :: String
  , oExpandKeys :: Bool
  , oRemoveUserData :: Bool
  , oBallparkStmtsPerFile :: Int
  , oIncludes :: Set FilePath
  }
  deriving (Show, Eq, Ord)

fixOpts :: Options -> IO Options
fixOpts opts
  | null $ oOutDir opts = quit "No output dir was supplied."
  | otherwise = do
      wd <- getCurrentDirectory
      let oDir = wd </> oOutDir opts
      return opts {
          oOutDir = oDir
        , oExternalizeDir = oDir </> "gen_data_files"
        }
  where
    quit msg = putStrLn msg >> exitFailure

toBool :: String -> Bool
toBool "0" = False
toBool "1" = True
toBool s = error $ "Cannot convert " ++ show s ++ " to a boolean."

parseArgs :: [String] -> Maybe ([FilePath], Options)
parseArgs args
  | "--zero" `elem` args = p args zeroedOptions
  | otherwise = p args baseOptions

baseOptions :: Options
baseOptions = Options {
    oOutDir = ""
  , oExternalizeDir = ""
  , oExternalizeData = True
  , oMergeSegs = True
  , oFlattenSegs = True
  , oRemoveNopOpenClose = True
  , oAnonPrefix = "$"
  , oExpandKeys = True
  , oRemoveUserData = True
  , oBallparkStmtsPerFile = 4000
  , oIncludes = Set.empty
  }

zeroedOptions :: Options
zeroedOptions = baseOptions {
    oExternalizeData = False
  , oMergeSegs = False
  , oFlattenSegs = False
  , oRemoveNopOpenClose = False
  , oExpandKeys = False
  , oRemoveUserData = False
  }

p :: [String] -> Options -> Maybe ([FilePath], Options)
p arguments opts = case arguments of
  "--zero" : args -> p args opts -- this arg is handled specially in parseArgs
  "-o" : dir : args -> p args opts { oOutDir = dir }
  "--out" : dir : args -> p args opts { oOutDir = dir }
  "--size" : n : args -> p args opts { oBallparkStmtsPerFile = read n }
  "-e" : b : args -> p args opts { oExternalizeData = toBool b }
  "--externalize-data" : b : args -> p args opts { oExternalizeData = toBool b }
  "--user-data" : b : args -> p args opts { oRemoveUserData = toBool b }
  "--nop" : b : args -> p args opts { oRemoveNopOpenClose = toBool b }
  "-f" : b : args -> p args opts { oFlattenSegs = toBool b }
  "--flatten" : b : args -> p args opts { oFlattenSegs = toBool b }
  "-k" : b : args -> p args opts { oExpandKeys = toBool b }
  "--expand-keys" : b : args -> p args opts { oExpandKeys = toBool b }
  "--anon" : prefix : args -> p args opts { oAnonPrefix = prefix }
  "-m" : b : args -> p args opts { oMergeSegs = toBool b }
  "--merge" : b : args -> p args opts { oMergeSegs = toBool b }
  "--incl" : path : args -> p args opts { oIncludes = Set.insert path $ oIncludes opts }
  files -> if any ("-" `isPrefixOf`) files
    then Nothing
    else Just (sortBy fileOrder files, opts)




data FileNumber = FileNumber Int (Maybe Int)
  deriving (Show, Eq, Ord)


getFileNum :: FilePath -> FileNumber
getFileNum = getFileNum' . span isNumber . reverse . dropExtension

getFileNum' :: (String, String) -> FileNumber
getFileNum' ("", _) = FileNumber (-1) Nothing
getFileNum' (rn, '_':cs) = getFileNum'' rn $ takeWhile isNumber cs
getFileNum' (rn, _) = FileNumber (read $ reverse rn) Nothing

getFileNum'' :: String -> String -> FileNumber
getFileNum'' rn "" = FileNumber (read $ reverse rn) Nothing
getFileNum'' rn2 rn1 = FileNumber n1 $ Just n2
  where
    n1 = read $ reverse rn1
    n2 = read $ reverse rn2

fileOrder :: FilePath -> FilePath -> Ordering
fileOrder f1 f2 = case comparing getFileNum f1 f2 of
  LT -> LT
  GT -> GT
  EQ -> compare f1 f2



