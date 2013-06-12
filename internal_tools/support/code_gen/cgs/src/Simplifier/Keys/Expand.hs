{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Keys.Expand (
  expandKeys
) where

import qualified C.Lexer.Compile.TH as L
import Control.Monad.State.Strict (State, runState, modify, gets, forM_)
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Scope (Scope(..))
import qualified Hoops.HC as HC
import qualified Hoops.HC.View as V
import Hoops.Key (Key, keyReps, asLookup)
import Hoops.Quote (hc)
import Hoops.SegPath (WildSegPath, SegPath, (<+>), mkPath, isAbsolute, isAnonymous, isAlias, expandAlias, expandPathKeys)
import Prelude hiding (error)
import qualified Prelude
import Token (Token, GenToken(..))
import qualified Token.String as TS

--------------------------------------------------------------------------------
-- Error Functions
--------------------------------------------------------------------------------

error :: String -> String -> a
error func msg = Prelude.error $ "Expand." ++ func ++ ": " ++ msg

errorM :: (Monad m) => String -> m String -> m a
errorM func mMsg = do
  msg <- mMsg
  error func msg

--------------------------------------------------------------------------------
-- Debug Functions
--------------------------------------------------------------------------------

getCloseKeyMatches :: Key -> Expander [(Key, Scope SegPath, SegPath)]
getCloseKeyMatches k = do
  gets $ filter ((k ==) . fst3) . map toTriple . Map.toList . keyMap
  where
    toTriple ((x, y), z) = (x, y, z)
    fst3 (x, _, _) = x

--------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------

data Segment
  = FullyPathed SegPath
  | PartlyPathed SegPath
  | Keyed Key
  deriving (Show, Eq, Ord)

data PathSource 
  = Create
  | Open 
  deriving (Show, Eq, Ord)

type Expander = State ExpandState

data ExpandState = ExpandState {
    openStack :: [Segment]
  , keyMap :: Map (Key, Scope SegPath) SegPath
  , aliasMap :: Map SegPath SegPath
  , anonymousNames :: [String]
  }

instance Show ExpandState where
  show st = concat [
      "ExpandState { openStack = "
    , show $ openStack st
    , ", keyMap = "
    , show $ keyMap st
    , ", aliasMap = "
    , show $ aliasMap st
    , ", anonymousNames = "
    , case anonymousNames st of
        [] -> "[]"
        front : _ -> "[" ++ show front ++ "..]"
    , " }"
    ]

mkExpandState :: String -> ExpandState
mkExpandState anonymousPrefix = ExpandState {
    openStack = []
  , keyMap = Map.empty
  , aliasMap = Map.empty
  , anonymousNames = [anonymousPrefix ++ show n | n <- [0::Int .. ]]
  }

--------------------------------------------------------------------------------
-- General Utility Functions
--------------------------------------------------------------------------------

infixr 5 <:>
(<:>) :: Functor f => a -> f [a] -> f [a]
(<:>) x = fmap (x :)

infixr 5 <++>
(<++>) :: Functor f => [a] -> f [a] -> f [a]
(<++>) x = fmap (x ++)

--------------------------------------------------------------------------------
-- Expander Utility Functions
--------------------------------------------------------------------------------

getOpenedSeg :: Expander (Maybe Segment)
getOpenedSeg = gets $ listToMaybe . openStack

getOpenedSegPath :: Expander (Maybe SegPath)
getOpenedSegPath = do
  mSeg <- getOpenedSeg
  case mSeg of
    Nothing -> return Nothing
    Just seg -> case seg of
      FullyPathed path -> return $ Just path
      PartlyPathed path -> return $ Just path
      Keyed _ -> return Nothing

getOpenedSegFullPath :: Expander (Maybe SegPath)
getOpenedSegFullPath = do
  mSeg <- getOpenedSeg
  case mSeg of
    Nothing -> return Nothing
    Just seg -> case seg of
      FullyPathed path -> return $ Just path
      PartlyPathed _ -> return Nothing
      Keyed _ -> return Nothing

newAnonName :: Expander String
newAnonName = do
  name : rest <- gets anonymousNames
  modify $ \st -> st { anonymousNames = rest }
  return name

getAbsolutePath :: SegPath -> Expander SegPath
getAbsolutePath path = let
  in if isAnonymous path
    then do
      anonName <- newAnonName
      getAbsolutePath $ mkPath $ TS.pack anonName
    else if isAbsolute path /= Just False
      then do
        am <- gets aliasMap
        km <- gets keyMap
        mCurr <- getOpenedSegPath
        let path' = expandAlias am path
        case expandPathKeys km mCurr path' of
          Nothing -> error "getAbsolutePath" $ "Could not expand string keys in path " ++ show path'
          Just path'' -> return path''
      else do
        mCurr <- getOpenedSegPath
        case mCurr of
          Nothing -> error "getAbsolutePath" $ "Cannot expand relative path with no open segments: " ++ show path
          Just curr -> getAbsolutePath $ curr <+> path

userKeyToFullPath :: Integer -> Expander SegPath
userKeyToFullPath k = do
  results <- fmap catMaybes $ mapM keyToMaybeFullPath keys
  case results of
    path : _ -> return path
    [] -> doesNotMapError
  where
    doesNotMapError = error "userKeyToPath" $ show k ++ " does not map to a segment or is not user defined."
    keys = keyReps k

keyToMaybeFullPath :: Key -> Expander (Maybe SegPath)
keyToMaybeFullPath key = do
  mOpenSeg <- getOpenedSegFullPath -- should this be able to handle getOpenedSeg if I modify keyMap's type to use Segment instead of SegPath? I don't think this is a good idea. Too complex
  case mOpenSeg of
    Nothing -> gets $ Map.lookup (key, Global) . keyMap
    Just openSeg -> do
      mResult <- gets $ Map.lookup (key, Local openSeg) . keyMap
      case mResult of
        Just result -> return $ Just result
        Nothing -> gets $ Map.lookup (key, Global) . keyMap

keyToFullPath :: Key -> Expander SegPath
keyToFullPath key = do
  mPath <- keyToMaybeFullPath key
  case mPath of
    Just path -> return path
    Nothing -> errorM "keyToPath" $ do
      closeMatches <- getCloseKeyMatches key
      mOpen <- getOpenedSegPath
      return $ unlines $ [
          show key ++ " does not map to a segment."
        , case mOpen of
            Just open -> "Opened segment is " ++ show open
            Nothing -> "There are no opened segments."
        , show (length closeMatches) ++ " close matches are: "
        ] ++ map (\(k, s, p) -> unwords [show k, show s, " -> ", show p]) closeMatches

ensureAbsolute :: SegPath -> Expander ()
ensureAbsolute path = case isAbsolute path of
  Just True -> return ()
  Just False -> error "ensureAbsolute" $ "Relative path: " ++ show path
  Nothing -> error "ensureAbsolute" $ "Might not be absolute: " ++ show path

ensureAlias :: SegPath -> Expander ()
ensureAlias path = if isAlias path
  then return ()
  else error "ensureAlias" $ "not an alias: " ++ show path

registerKey :: Key -> Scope SegPath -> SegPath -> Expander ()
registerKey key scope fullPath = do
  ensureAbsolute fullPath
  modify $ \st -> let
    keyMap' = Map.insert (key, scope) fullPath $ keyMap st
    in st { keyMap = keyMap' }

registerAlias :: SegPath -> SegPath -> Expander ()
registerAlias alias expansion = do
  ensureAlias alias
  modify $ \st -> let
    aliasMap' = Map.insert alias expansion $ aliasMap st
    in st { aliasMap = aliasMap' }

pushOpenStack :: Segment -> Expander ()
pushOpenStack mFullPath = case mFullPath of
  FullyPathed fullPath -> do
    ensureAbsolute fullPath
    modify $ \st -> st { openStack = FullyPathed fullPath : openStack st }
  PartlyPathed path -> do
    modify $ \st -> st { openStack = PartlyPathed path : openStack st }
  Keyed key -> do
    modify $ \st -> st { openStack = Keyed key : openStack st }

popOpenStack :: Expander ()
popOpenStack =  modify $ \st -> let
  os = case openStack st of
    [] -> error "popOpenStack" "No open segments to close."
    _ : segs -> segs
  in st { openStack = os }

strToScope :: (Key, Integer) -> TS.StringType -> Expander (Scope SegPath)
strToScope (oldKey, newKey) scopeStr = case fmap (toLower . fst) $ TS.uncons scopeStr of
  Just 'g' -> return Global
  Just 'l' -> do
    mOpenPath <- getOpenedSegPath
    case mOpenPath of
      Just openPath -> return $ Local openPath
      Nothing -> error "strToScope" $ "Cannot locally renumber " ++ show oldKey ++ " to " ++ show newKey ++ " with no opened segments."
  _ -> error "strToScope" $ "Illegal scope string: " ++ show scopeStr

--------------------------------------------------------------------------------
-- Expander
--------------------------------------------------------------------------------

expandKeys :: String -> [Token] -> [Token]
expandKeys anonPrefix = uncurry (flip seq) . flip runState st . expand
  where
    st = mkExpandState anonPrefix

expand :: [Token] -> Expander [Token]
expand (V.openSegment -> Just (path, mKey, ts)) = handlePathSource Open path mKey ts
expand (V.openSegmentByKey -> Just (key, ts)) = handleOpenByKey key ts
--expand (V.openSegmentKeyByKey -> Just (key, ts)) =
expand (V.closeSegment -> Just ts) = handleClose ts
expand (V.createSegment -> Just (path, mKey, ts)) = handlePathSource Create path mKey ts
expand (V.renumberKey -> Just (old, new, scope, mKey, ts)) = handleRenumber old new scope mKey ts
expand (V.defineAlias -> Just (alias, expansion, ts)) = handleDefineAlias alias expansion ts
--expand (V.moveSegment -> ) =
--expand (V.renameSegment -> ) =
--expand (V.moveByKey -> ) =
--expand (V.moveKeyByKey -> ) =
expand [hc| @k | key ts |] = handleGenericKey key ts
expand (t : ts) = t <:> expand ts
expand [] = return []

--------------------------------------------------------------------------------
-- HC Handlers
--------------------------------------------------------------------------------

handleClose :: [Token] -> Expander [Token]
handleClose ts = do
  popOpenStack
  HC.closeSegment <++> expand ts

handleDefineAlias :: SegPath -> SegPath -> [Token] -> Expander [Token]
handleDefineAlias alias expansion ts = do
  registerAlias alias expansion
  HC.defineAlias alias expansion <++> expand ts

handleOpenByKey :: Key -> [Token] -> Expander [Token]
handleOpenByKey key ts = do
  mFullPath <- keyToMaybeFullPath key
  case mFullPath of
    Just fullPath -> do
      pushOpenStack $ FullyPathed fullPath
      HC.openSegment fullPath Nothing <++> expand ts
    Nothing -> do
      pushOpenStack $ Keyed key
      HC.openSegmentByKey key <++> expand ts

handlePathSource :: PathSource -> SegPath -> Maybe Key -> [Token] -> Expander [Token]
handlePathSource source path mKey ts = do
  mSeg <- getOpenedSeg
  case mSeg of
    Nothing -> goFullyExpand
    Just segment -> case segment of
      FullyPathed _ -> goFullyExpand
      PartlyPathed _ -> if isAbsolute path == Just True
        then goFullyExpand
        else goPartlyExpand
      Keyed _ -> goPartlyExpand
  where
    hcFunc = case source of
      Open -> HC.openSegment
      Create -> HC.createSegment
    goFullyExpand = do
      fullPath <- getAbsolutePath path
      case source of
        Create -> return ()
        Open -> pushOpenStack $ FullyPathed fullPath
      case mKey of
        Just key -> registerKey key Global fullPath
        Nothing -> return ()
      hcFunc fullPath mKey <++> expand ts
    goPartlyExpand = do
      case source of
        Create -> return ()
        Open -> pushOpenStack $ PartlyPathed path
      --Do not register the key because we have no full path to register it to
      hcFunc path mKey <++> expand ts

handleRenumber :: Key -> Integer -> TS.StringType -> Maybe Key -> [Token] -> Expander [Token]
handleRenumber old new scopeStr mDefinedKey ts = do
  validateDefinedKey
  mFullPath <- keyToMaybeFullPath old
  case mFullPath of
    Nothing -> return () -- Likely not a segment (say it was never explicitly defined, such as by Find_Contents). Likely geometry
    Just fullPath -> do
      scope <- strToScope (old, new) scopeStr
      forM_ newKeys $ \newKey -> registerKey newKey scope fullPath
  HC.renumberKey old new scopeStr mDefinedKey <++> expand ts
  where
    newKeys = keyReps new
    validateDefinedKey = case mDefinedKey of
      Nothing -> return ()
      Just definedKey -> if definedKey `elem` newKeys
        then return ()
        else error "handleRenumber" $ "DEFINE'd key differs from supplied key: "
          ++ "Supplied is " ++ show new
          ++ ", DEFINE'd is " ++ show definedKey

handleGenericKey :: Key -> [Token] -> Expander [Token]
handleGenericKey key ts = do
  mPath <- keyToMaybeFullPath key
  let keyToks = case mPath of
        Nothing -> asLookup key -- likely not a segment... probably geometry
        Just path -> $(L.compile "K(") ++ TString (TS.show path) : $(L.compile ")")
  keyToks <++> expand ts



