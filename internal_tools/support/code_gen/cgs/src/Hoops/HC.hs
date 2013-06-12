{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hoops.HC (
  closeSegment
, conditionalInclude
, createSegment
, defineAlias
, deleteSegment
, includeSegment
, moveSegment
, openSegment
, openSegmentByKey
, renameSegment
, renumberKey
) where

import qualified C.Lexer.Compile.TH as L
import Data.List (intersperse)
import Hoops.Key (Key, asDefine, asLookup)
import Hoops.SegPath (SegPath, WildSegPath)
import Token (Token, GenToken(..))
import Token.Number (Number(..))
import qualified Token.String as TS

--------------------------------------------------------------------------------

-- instead of (comp = L.compile) because of TH module restrictions
#define COMP(s) $(L.compile s)

--------------------------------------------------------------------------------

close :: TS.StringType -> [Token]
close name = hcFunc : COMP("();")
  where
    hcFunc = TIdentifier $ "HC_Close_" `TS.append` name

closeSegment :: [Token]
closeSegment = COMP("HC_Close_Segment();")

conditionalInclude :: SegPath -> TS.StringType -> Maybe Key -> [Token]
conditionalInclude path cond Nothing 
  = COMP("HC_Conditional_Include(") ++ TString (TS.show path) : COMP(",") ++ TString cond : COMP(");")
conditionalInclude path cond (Just key) 
  = asDefine key (init $ conditionalInclude path cond Nothing) ++ COMP(";")

createSegment :: SegPath -> Maybe Key -> [Token]
createSegment path Nothing = COMP("HC_Create_Segment(") ++ TString (TS.show path) : COMP(");")
createSegment path (Just key) = asDefine key (init $ createSegment path Nothing) ++ COMP(";")

defineAlias :: SegPath -> SegPath -> [Token]
defineAlias alias expansion
  = COMP("HC_Define_Alias(") ++ TString (TS.show alias) : COMP(",") ++ TString (TS.show expansion) : COMP(");")

deleteSegment :: SegPath -> [Token]
deleteSegment path = COMP("HC_Delete_Segment(") ++ TString (TS.show path) : COMP(");")

includeSegment :: SegPath -> Maybe Key -> [Token]
includeSegment path Nothing = COMP("HC_Include_Segment(") ++ TString (TS.show path) : COMP(");")
includeSegment path (Just key) = asDefine key (init $ includeSegment path Nothing) ++ COMP(";")

moveSegment :: WildSegPath -> SegPath -> [Token]
moveSegment wpath path = COMP("HC_Move_Segment(") ++ TString (TS.show wpath) : COMP(",") ++ TString (TS.show path) : COMP(");")

open :: TS.StringType -> [Token] -> [Token]
open name args = hcFunc : TSymbol "(" : argList ++ COMP(");")
  where
    hcFunc = TIdentifier $ "HC_Open_" `TS.append` name
    argList = intersperse (TSymbol ",") args

openSegment :: SegPath -> Maybe Key -> [Token]
openSegment path Nothing = COMP("HC_Open_Segment(") ++ TString (TS.show path) : COMP(");")
openSegment path (Just key) = asDefine key (init $ openSegment path Nothing) ++ COMP(";")

openSegmentByKey :: Key -> [Token]
openSegmentByKey key = COMP("HC_Open_Segment_By_Key(") ++ asLookup key ++ COMP(");")

renameSegment :: WildSegPath -> SegPath -> [Token]
renameSegment = moveSegment

renumberKey :: Key -> Integer -> TS.StringType -> Maybe Key -> [Token]
renumberKey old new scope Nothing 
  = COMP("HC_Renumber_Key(") ++ asLookup old ++ COMP(",") ++ TNumber (TInteger new) 
  : COMP(",") ++ TString scope : COMP(");")
renumberKey old new scope (Just key) = asDefine key (init $ renumberKey old new scope Nothing) ++ COMP(";")


