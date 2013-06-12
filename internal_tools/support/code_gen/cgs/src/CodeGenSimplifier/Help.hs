module CodeGenSimplifier.Help (
  help
) where

import CodeGenSimplifier.OptionHelp (OptionHelp(..), optionHelp, formatText, formatOption)
import CodeGenSimplifier.Options (Options(..), baseOptions)
import System.Environment (getProgName)
import System.FilePath (takeBaseName)

--------------------------------------------------------------------------------

opt :: Maybe Char -> String -> String -> IO ()
opt mShort long = putStrLn . formatOption optionHelp mShort (Just long)

put :: String -> IO ()
put = putStrLn . formatText oh
  where
    oh = optionHelp { optCol = 0, descCol = 0 }

sectionDelim :: String
sectionDelim = ""

class OptionsShow a where
  optionsShow :: a -> String

instance OptionsShow Int where
  optionsShow = show

instance OptionsShow Bool where
  optionsShow True = "1"
  optionsShow False = "0"

showDefault :: OptionsShow a => (Options -> a) -> String
showDefault sel = "Defaults to " ++ optionsShow (sel baseOptions) ++ "."


help :: IO ()
help = do
  progName <- getProgName
  put $ "Usage: " ++ takeBaseName progName ++ " [OPTIONS] FILES"
  put sectionDelim
  put $ unwords [
        "All input files must be in the same directory."
      , "Input files are processed in numerical order decided by the"
      , "trailing number in the file name before the file extension."
      , "Files without such numbers are treated as having the"
      , "numeric value of -1."
      , "Files are of the form A, AX,"
      , "or AX_Y, where X and Y are numbers and Y is a subnumber of X."
      , "X comes before X_Y for all X and Y."
      , "X and Y are non-negative integers."
      , "Ties are decided lexigraphically."
      , "Output files are of the form 'codeX.cpp',"
      , "where X begins at 0 and increments by 1."
      ]
  put sectionDelim
  put "Types:"
  put "BOOL is either a 0 or a 1."
  put sectionDelim
  put "Options:"
  opt (Just 'h') "help" "Show this help dialog."
  opt (Just 'o') "out DIR" $ unwords [
        "All simplified files are outputted to DIR."
      , "This option is required."
      ]
  opt Nothing "size INT" $ unwords [
        "Ballpark number of statements per output file."
      , showDefault oBallparkStmtsPerFile
      ]
  opt Nothing "zero" "Makes all BOOL options default to 0."
  opt (Just 'e') "externalize-data BOOL" $ unwords [
        "If BOOL, move object data to external files and insert code to"
      , "load the data."
      , showDefault oExternalizeData
      ]
  opt Nothing "incl PATH" $ unwords [
        "Includes PATH in shared.h. PATH is included in double quotes."
      ]
  opt Nothing "user-data BOOL" $ unwords [
        "If BOOL, remove user data."
      , showDefault oRemoveUserData
      ]
  opt Nothing "nop BOOL" $ unwords [
        "Remove NOP open/close pairs and NOP create segments."
      , "Will keep ones with meaningful DEFINE's."
      , showDefault oRemoveNopOpenClose
      ]
  opt (Just 'f') "flatten BOOL" $ unwords [
        "Flatten segment structure so that if possible,"
      , "no more than one segment is open at a time. In cases where this is"
      , "not possible, then structure is flattened as much as possible."
      , showDefault oFlattenSegs
      ]
  opt (Just 'k') "expand-keys BOOL" $ unwords [
        "Expand segment keys to segment names."
      , showDefault oExpandKeys
      ]
  opt Nothing "anon PREFIX" $ unwords [
        "When expanding keys via --expand-keys,"
      , "expand anonymous segment names to PREFIX[N], where"
      , "[N] is a non-negative integer that is unique for each"
      , "anonymous segment."
      , "Default is `" ++ oAnonPrefix baseOptions ++ "` without quotes."
      ]
  opt (Just 'm') "merge" $ unwords [
        "Merge adjacent open/close pairs if they belong to the same segment."
      , showDefault oMergeSegs
      ]


