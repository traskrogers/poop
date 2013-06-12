module Data.FilePrototype (
  FilePrototype
, Dir
, Name
, Ext
, mkFilePrototype
, nextFile
) where

import System.FilePath ((<.>), (</>))

--------------------------------------------------------------------------------

type Dir = String
type Name = String
type Ext = String

data FilePrototype = FP {
    fileDir :: String
  , fileName :: String
  , fileExt :: String
  , fileNum :: !Int
  }
  deriving (Show)

mkFilePrototype :: Dir -> Name -> Ext -> FilePrototype
mkFilePrototype dir name ext = FP {
    fileDir = dir
  , fileName = name
  , fileExt = ext
  , fileNum = -1
  }

nextFile :: FilePrototype -> (FilePath, FilePrototype)
nextFile fp = (path, fp { fileNum = n })
  where
    n = fileNum fp + 1
    path = fileDir fp </> fileName fp ++ show n <.> fileExt fp



