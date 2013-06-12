{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Externalize (
  externalizeData
) where

import Control.Monad (when)
import Control.Monad.State.Strict (put)
import Data.Char (toLower)
import Data.FilePrototype (FilePrototype, Dir, mkFilePrototype, nextFile)
import Data.String (IsString(fromString))
import Filesystem (createTree)
import Hoops.HC.View (leafBlock)
import Hoops.Key (Key, asDefine)
import qualified Simplifier.Externalize.FaceNormals as FaceNormals
import qualified Simplifier.Externalize.Polygon as Polygon
import qualified Simplifier.Externalize.Polyline as Polyline
import qualified Simplifier.Externalize.Shell as Shell
import qualified Simplifier.Externalize.ShellByTristrips as ShellByTristrips
import qualified Simplifier.Externalize.VertexColorsByFIndex as VertexColorsByFIndex
import qualified Simplifier.Externalize.VertexNormals as VertexNormals
import Token (Token, GenToken(..))
import qualified Token.String as TS

--------------------------------------------------------------------------------

type FileNum = Int
type MinStmtCnt = Int

externalizeData :: Dir -> FileNum -> [Token] -> IO [Token]
externalizeData dir fileNum ts = do
  createTree $ fromString dir
  return ts
    >>= run "hmf" Shell.func Shell.writer
    >>= run' "norm" VertexNormals.func VertexNormals.reader VertexNormals.writer
    >>= run' "norm" FaceNormals.func FaceNormals.reader FaceNormals.writer
    >>= run "hmf" Polygon.func Polygon.writer
    >>= run "hmf" Polyline.func Polyline.writer
    >>= run "hmf" ShellByTristrips.func ShellByTristrips.writer
    >>= run' "fidx" VertexColorsByFIndex.func VertexColorsByFIndex.reader VertexColorsByFIndex.writer
  where
    run ext writer = run' ext writer readMetafile
    run' ext funcName reader writer ts' = do
      let funcName' = map toLower funcName
      let fp = mkFilePrototype dir (funcName' ++ "_" ++ show fileNum ++ "_") ext
      putStrLn $ "externalizing " ++ funcName ++ "..."
      externalize (TS.pack funcName) reader writer fp ts'

--------------------------------------------------------------------------------

infixr 5 <:>
(<:>) :: Functor f => a -> f [a] -> f [a]
(<:>) x = fmap (x :)

infixr 5 <++>
(<++>) :: Functor f => [a] -> f [a] -> f [a]
(<++>) x = fmap (x ++)

externalize
    :: TS.StringType
    -> (Maybe Key -> FilePath -> [Token])
    -> (FilePath -> [Token] -> IO (Maybe Key))
    -> FilePrototype 
    -> [Token] 
    -> IO [Token]
externalize funcName reader writer = ext
  where
    view = leafBlock False $ \t -> case t of
      TIdentifier name -> when (funcName == name) (put True)
      _ -> return ()
    ext fp (view -> Just (block, True, ts)) = do
      let (path, fp') = nextFile fp
      mKey <- writer path block
      let gRead = reader mKey path
      gRead <++> ext fp' ts
    ext fp (t:ts) = t <:> ext fp ts
    ext _ [] = return []

--------------------------------------------------------------------------------

readMetafile :: Maybe Key -> FilePath -> [Token]
readMetafile (Just key) = gReadMetafile key
readMetafile Nothing = hcReadMetafile

gReadMetafile :: Key -> FilePath -> [Token]
gReadMetafile key path = asDefine key [
    TIdentifier "G_Read_Metafile"
  , TSymbol "("
  , TString $ TS.pack path
  , TSymbol ")"
  ] ++ [
    TSymbol ";"
  ]

hcReadMetafile :: FilePath -> [Token]
hcReadMetafile path = [
    TIdentifier "HC_Read_Metafile"
  , TSymbol "("
  , TString $ TS.pack path
  , TSymbol ","
  , TString "."
  , TSymbol ","
  , TString ""
  , TSymbol ")"
  , TSymbol ";"
  ]


