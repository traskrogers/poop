{-# LANGUAGE TemplateHaskell #-}

module Token.Keyword (
  Keyword(..)
, keywords
) where

import C.Show (CShow(..))
import Data.Binary
import Data.Char (isLower, isUpper, toLower)
import Data.List (intercalate)
import Language.Haskell.TH (conE, mkName)
import Language.Haskell.TH.Syntax (Lift(..))

--------------------------------------------------------------------------------

data Keyword
  = KAuto | KBool | KBreak | KCase | KCatch | KChar | KClass | KConst
  | KConstCast | KContinue | KDefault | KDelete | KDo | KDouble
  | KDynamicCast | KElse | KEnum | KExplicit | KExtern | KFinally
  | KFloat | KFor | KFriend | KGoto | KIf | KInline | KInt | KLong
  | KNamespace | KNew | KOperator | KPrivate | KProtected | KPublic
  | KRegister | KReinterpretCast | KReturn | KShort | KSigned | KSizeof
  | KStatic | KStaticCast | KStruct | KSwitch | KTemplate | KTypedef | KUnion
  | KUnsigned | KUsing | KVirtual | KVoid | KVolatile | KWcharT | KWhile
  deriving (Show, Read, Enum, Eq, Ord)

instance Lift Keyword where
  lift = conE . mkName . show

instance Binary Keyword where
  put = put . fromEnum
  get = fmap toEnum get

keywords :: [Keyword]
keywords = [KAuto ..]

splitByCase :: String -> [String]
splitByCase "" = []
splitByCase (c:cs)
  | isUpper c = (c : lowers) : splitByCase rest
  | otherwise = lowers : splitByCase rest
  where
    (lowers, rest) = span isLower cs

instance CShow Keyword where
  cshow = intercalate "_" . map (map toLower) . splitByCase . tail . show


