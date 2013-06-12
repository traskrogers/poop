{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Hoops.HC.View (
  defSt
, begDef
, endDef
, arg
, args
, args1
------------------
, leafBlock
, leafBlock_
------------------
, close
, closeSegment
, conditionalInclude
, createSegment
, defineAlias
, includeSegment
, moveSegment
, open
, openSegment
, openSegmentByKey
, renameSegment
, renumberKey
) where

import qualified C.Lexer.Compile.TH as L.TH
import Control.Monad (when)
import Control.Monad.ListM (spanM)
import Control.Monad.State.Strict (State, runState, evalState, get, put, modify)
import Data.List (stripPrefix)
import Hoops.Key (Key, key1, key2)
import Hoops.SegPath (SegPath, WildSegPath, mkPath, mkWildPath)
import Hoops.Quote (hc)
import Prelude hiding (error)
import qualified Prelude
import Token (Token, GenToken(..))
import Token.Number (Number(..))
import qualified Token.String as TS

--------------------------------------------------------------------------------

error :: String -> String -> a
error func msg = Prelude.error $ "Hoops.HC.View." ++ func ++ ": " ++ msg

--------------------------------------------------------------------------------

type Viewer st a = State st (Maybe a)

data DefineState = NoDefineExists | DefineExists

defSt :: DefineState
defSt = NoDefineExists

begDef :: [Token] -> Viewer DefineState ((), [Token])
begDef ts = case stripPrefix $(L.TH.compile "DEFINE(") ts of
  Just rest -> do
    put DefineExists
    return $ Just ((), rest)
  Nothing -> return $ Just ((), ts)

endDef :: [Token] -> Viewer DefineState (Maybe Key, [Token])
endDef ts = do
  d <- get
  case d of
    NoDefineExists -> return $ Just (Nothing, ts)
    DefineExists -> case ts of 
      TSymbol "," : TNumber (TInteger k1) : TSymbol "," : TNumber (TInteger k2) : TSymbol ")" : rest
            -> return $ Just (Just $ key2 k1 k2, rest)
      TSymbol "," : TNumber (TInteger k) : TSymbol ")" : rest 
            -> return $ Just (Just $ key1 k, rest)
      _ -> return Nothing

arg :: [Token] -> Maybe ([Token], [Token])
arg ts
  | null theArg = Nothing
  | otherwise = Just (theArg, rest)
  where
    (theArg, rest) = evalState (spanM p ts) (0 :: Int)
    p (TSymbol sym)
      | sym `elem` [",", ")"] = do
          n <- get
          if n == 0
            then return False
            else do
              when (sym == ")") $ modify (+ 1)
              return True
      | sym == "(" = do
          modify $ subtract 1
          return True
      | otherwise = return True
    p _ = return True

args :: [Token] -> Maybe ((), [Token])
args ts = case ts of
  TSymbol ")" : _ -> Just ((), ts)
  _ -> args1 ts

args1 :: [Token] -> Maybe ((), [Token])
args1 ts = case arg ts of
  Just (_, ts') -> args1' ts'
  Nothing -> Nothing

args1' :: [Token] -> Maybe ((), [Token])
args1' ts = case ts of
  TSymbol "," : ts' -> args1 ts'
  TSymbol ")" : _ -> Just ((), ts)
  _ -> Nothing

--------------------------------------------------------------------------------

notBraceM :: (Token -> State st ()) -> Token -> State st Bool
notBraceM f t = case t of
  TSymbol "{" -> return False
  TSymbol "}" -> return False
  _ -> f t >> return True

evalBlock :: st -> State st (a, a) -> ((a, st), a)
evalBlock st = reorder . flip runState st
  where
    reorder ((x, y), s) = ((x, s), y)

leafBlock :: st -> (Token -> State st ()) -> [Token] -> Maybe ([Token], st, [Token])
leafBlock st f [hc| { @[Just . evalBlock st . spanM (notBraceM f)] } | (block,st') ts |] = Just (block, st', ts)
leafBlock _ _ _ = Nothing

leafBlock_ :: [Token] -> Maybe ([Token], [Token])
leafBlock_ ts = case leafBlock () (const $ return ()) ts of
  Just (xs, _, ys) -> Just (xs, ys)
  Nothing -> Nothing

--------------------------------------------------------------------------------

close :: TS.StringType -> [Token] -> Maybe [Token]
close name [hc| @i(@[args]); | funcName ts |] = case TS.stripPrefix "HC_Close_" funcName of
  Just name' -> if maybe name id (TS.stripSuffix "_By_Key" name) == name'
    then Just ts
    else Nothing
  _ -> Nothing
close _ _ = Nothing

closeSegment :: [Token] -> Maybe [Token]
closeSegment [hc| HC_Close_Segment(); | ts |] = Just ts
closeSegment _ = Nothing

conditionalInclude :: [Token] -> Maybe (SegPath, TS.StringType, Maybe Key, [Token])
conditionalInclude [hc| :defSt: @{begDef}HC_Conditional_Include(@s,@s)@{endDef}; | s c mKey ts |]
  = Just (mkPath s, c, mKey, ts)
conditionalInclude _ = Nothing

createSegment :: [Token] -> Maybe (SegPath, Maybe Key, [Token])
createSegment [hc| :defSt: @{begDef}HC_Create_Segment(@s)@{endDef}; | s mKey ts |] = Just (mkPath s, mKey, ts)
createSegment _ = Nothing

defineAlias :: [Token] -> Maybe (SegPath, SegPath, [Token])
defineAlias [hc| HC_Define_Alias(@s,@s); | alias expansion ts |] = Just (mkPath alias, mkPath expansion, ts)
defineAlias _ = Nothing

includeSegment :: [Token] -> Maybe (SegPath, Maybe Key, [Token])
includeSegment [hc| :defSt: @{begDef}HC_Include_Segment(@s)@{endDef}; | s mKey ts |] = Just (mkPath s, mKey, ts)
includeSegment _ = Nothing

moveSegment :: [Token] -> Maybe (WildSegPath, WildSegPath, [Token])
moveSegment [hc| HC_Move_Segment(@s, @s); | s1 s2 ts |] = Just (mkWildPath s1, mkWildPath s2, ts)
moveSegment _ = Nothing

open :: [Token] -> Maybe (TS.StringType, [Token])
open [hc| @i(@[args]); | funcName ts |] = case TS.stripPrefix "HC_Open_" funcName of
  Just name -> Just (name, ts)
  _ -> Nothing
open _ = Nothing

openSegment :: [Token] -> Maybe (SegPath, Maybe Key, [Token])
openSegment [hc| :defSt: @{begDef}HC_Open_Segment(@s)@{endDef}; | s mKey ts |] = Just (mkPath s, mKey, ts)
openSegment _ = Nothing

openSegmentByKey :: [Token] -> Maybe (Key, [Token])
openSegmentByKey [hc| HC_Open_Segment_By_Key(@k); | k ts |] = Just (k, ts)
openSegmentByKey _ = Nothing

renameSegment :: [Token] -> Maybe (WildSegPath, WildSegPath, [Token])
renameSegment [hc| HC_Rename_Segment(@s, @s); | s1 s2 ts |] = Just (mkWildPath s1, mkWildPath s2, ts)
renameSegment _ = Nothing

renumberKey :: [Token] -> Maybe (Key, Integer, TS.StringType, Maybe Key, [Token])
renumberKey [hc| :defSt: @{begDef}HC_Renumber_Key(@k, @d, @s)@{endDef}; | old new scope mKey ts |]
  = Just (old, new, scope, mKey, ts)
renumberKey _ = Nothing


