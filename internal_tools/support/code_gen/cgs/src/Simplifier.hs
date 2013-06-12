{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier (
  simplify
) where

import C.Code.Main.Cpp (main_cpp)
import C.Code.Shared.H (shared_h)
import C.Code.Shared.Cpp (shared_cpp)
import C.Lexer (runLexer)
import qualified C.Lexer.Compile.TH as L
import CodeGenSimplifier.Options (Options(..))
import Control.DeepSeq (NFData, deepseq)
import Control.DeepSeq.Instances ()
import Control.Exception (evaluate)
import Control.Monad ((>=>), zipWithM_, forM, foldM)
import Control.Monad.ListM (spanM)
import Control.Monad.State.Strict (get, put, evalState)
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.FilePrototype (Dir)
import Data.Function.Extra (dot)
import Data.List (stripPrefix)
import Data.Represent (Representer, mkRepresenter, representAll, representatives)
import Data.String (IsString(..))
import Data.WildString ((=?), WildString(..))
import Filesystem (createTree)
import Hoops.HC.View (args)
import Hoops.Quote (hc)
import Simplifier.CGComment (removeCGComments)
import Simplifier.Database (removeDatabaseControl)
import Simplifier.Externalize (externalizeData)
import Simplifier.Keys.Define (removeExtraneousDefines)
import Simplifier.Keys.Expand (expandKeys)
import Simplifier.Matrix (shrinkMatrices)
import Simplifier.Nop (removeNopOpenCloses)
import Simplifier.Seg.Merge (mergeSegments)
import Simplifier.Seg.Flatten (flattenSegments)
import Simplifier.UserData (removeUserData)
import System.Directory (getTemporaryDirectory)
import System.Exit (exitFailure)
import System.IO.Temp (withTempDirectory)
import System.FilePath (takeDirectory, (</>))
import Token (Token, GenToken(..), isComment)
import Token.Keyword (Keyword(..))
import Token.ToCppCode (toCode)

--------------------------------------------------------------------------------

filenames :: Dir -> [FilePath]
filenames dir = [dir </> ("code" ++ show n ++ ".cpp") | n <- [0::Integer ..]]

withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir io = do
  tmp <- getTemporaryDirectory
  withTempDirectory tmp "cgs" io

type Rep = Representer Token

lexFile :: Maybe Rep -> FilePath -> IO ([Token], Rep)
lexFile mR file = do
  putStrLn $ "lexing " ++ file ++ "..."
  contents <- LC.readFile file
  case runLexer mR (Just file) contents of
    Left err -> do
      print err
      exitFailure
    Right res -> return res

writeTokens :: FilePath -> [Token] -> IO ()
writeTokens path = LB.writeFile path . encode

readTokens :: FilePath -> IO [Token]
readTokens = fmap decode . LB.readFile

lexFiles :: Maybe Rep -> [FilePath] -> IO [[Token]]
lexFiles _ [] = return []
lexFiles mR (file:files) = do
  (ts, r) <- lexFile mR file
  fmap (ts :) $ lexFiles (Just r) files

lexFilesAndExternalizeData :: Options -> [FilePath] -> IO [[Token]]
lexFilesAndExternalizeData opts filePaths = withTmpDir $ \tmp -> do
  tmpFilePaths <- forM (zip filePaths [0..]) $ \(filePath, n) -> do
    (ts, _) <- lexFile Nothing filePath
    ts' <- externalizeData (oExternalizeDir opts) n ts
    let tmpFilePath = tmp </> show n
    writeTokens tmpFilePath ts'
    return tmpFilePath
  (toks, _) <- (\io -> foldM io ([], mkRepresenter id) $ reverse tmpFilePaths) $
    \(toksTail, rep) file -> do
      toks <- readTokens file
      let (toks', rep') = representAll toks rep
      forceIO toks'
      return (toks' : toksTail, rep')
  return toks

--------------------------------------------------------------------------------

simplify :: Options -> [FilePath] -> IO ()
simplify opts files = do
  createTree $ fromString outDir
  tss <- if oExternalizeData opts
    then lexFilesAndExternalizeData opts files
    else lexFiles Nothing files
  ts <- simplify' opts $ extractCodeGen tss
  putStrLn "computing output files..."
  let codes = toCode $ splitCode ballparkStmts ts
  zipWithM_ io (filenames outDir) codes
  putStrLn "writing shared.h..."
  writeFile (outDir </> "shared.h") $ shared_h opts
  putStrLn "writing shared.cpp..."
  writeFile (outDir </> "shared.cpp") shared_cpp
  putStrLn "writing main.cpp..."
  writeFile (outDir </> "main.cpp") $ main_cpp $ length codes
  putStrLn "DONE."
  where
    ballparkStmts = oBallparkStmtsPerFile opts
    outDir = oOutDir opts
    io file code = do
      putStrLn $ "writing " ++ file ++ "..."
      createTree $ fromString $ takeDirectory file
      writeFile file code

simplify' :: Options -> [Token] -> IO [Token]
simplify' opts = return
  >=> run "removing code gen comments..." removeCGComments
  >=> run "removing database code..." removeDatabaseControl
  >=> run "shrinking matrices..." shrinkMatrices
  >=> try "removing user data..." oRemoveUserData removeUserData
  >=> try "expanding keys..." oExpandKeys (expandKeys anonPrefix)
  >=> try "flattening segments..." oFlattenSegs flattenSegments
  >=> run "removing extraneous defines..." removeExtraneousDefines
  >=> try "removing NOP open close pairs..." oRemoveNopOpenClose removeNopOpenCloses
  >=> try "merging segments..." oMergeSegs mergeSegments
  where
    anonPrefix = oAnonPrefix opts
    run msg = try msg $ const True
    try msg p f = if p opts
      then \ts -> do
        putStrLn msg
        let ts' = representatives $ f ts
        forceIO ts'
        return ts'
      else return

forceIO :: NFData a => a -> IO ()
forceIO x = evaluate (x `deepseq` x) >> return ()

--------------------------------------------------------------------------------

type NumStmts = Int
data SplitSt = SplitSt { stmts :: Int, open :: Int }

splitCode :: NumStmts -> [Token] -> [[Token]]
splitCode = filter (not . null) `dot` splitCode'

splitCode' :: NumStmts -> [Token] -> [[Token]]
splitCode' _ [] = []
splitCode' n ts = xs : splitCode' n ys
  where
    (xs, ys) = evalState (spanM p ts) $ SplitSt { stmts = 0, open = 0 }
    p t = do
      st <- get
      if stmts st >= n && open st == 0
        then return False
        else let
          st' = case t of
            TSymbol ";" -> st { stmts = stmts st + 1 }
            TSymbol "{" -> st { open = open st + 1 }
            TSymbol "}" -> st { open = open st - 1 }
            _ -> st
          in do
            put st'
            return True

extractCodeGen :: [[Token]] -> [Token]
extractCodeGen = concatMap $ extractCodeGen' . filter (not . isComment)

extractCodeGen' :: [Token] -> [Token]
extractCodeGen' (viewEntry -> Just ts) = takeWhile p $ removeCGVarDecls ts
  where
    p (TIdentifier i) = not $ i =? Wild "code_chain_##*"
    p (TKeyword KReturn) = False
    p _ = True
extractCodeGen' (_:ts) = extractCodeGen' ts
extractCodeGen' [] = error "Cannot find code gen sequence."

viewEntry :: [Token] -> Maybe [Token]
viewEntry [hc| int @i(@[args]){ | i ts |]
  | i == "main" || i =? Wild "code_chain_##*" = Just ts
  | otherwise = Nothing
viewEntry _ = Nothing

removeCGVarDecls :: [Token] -> [Token]
removeCGVarDecls (stripPrefix functionDecls -> Just ts) = ts
removeCGVarDecls ts = ts

functionDecls :: [Token]
functionDecls = $(L.compile $ concat [
        "char string_buffer[256];"
      , "float ff;"
      , "int ii;"
      , "long ll;"
      , "HC_KEY key;"
      , "float matrix[16];"
      ])


