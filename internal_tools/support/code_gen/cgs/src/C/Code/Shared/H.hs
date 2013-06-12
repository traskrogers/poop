module C.Code.Shared.H (
  shared_h
) where

import CodeGenSimplifier.Options (Options(..))
import qualified Data.Set as Set

--------------------------------------------------------------------------------

shared_h :: Options -> String
shared_h opts = unlines [
    "#pragma once"
  , ""
  , hcStructs
  , hcDefines
  , userIncludes opts
  , includes
  , envStruct
  , globals
  , k
  , defineKey
  , lookupKey
  , gWriteMatrix
  , gReadFaceNormals
  , gReadVertexNormals
  , gReadVertexColorsByFIndex
  , gInsertMetafile
  , gDelete
  , gFlush
  , gMove
  , gControlUpdate
  , gIncludeSegment
  , gStyleSegment
  , gConditionalStyle
  , gBringToFront
  , gConditionalReference
  , ""
  ]

hcStructs :: String
hcStructs = unlines [
    "typedef struct {float x, y, z;} Point;"
  , "typedef struct {float x, y, z;} Vector;"
  , "typedef struct {float a, b, c, d;} Plane;"
  , "typedef struct {float red, green, blue;} RGB;"
  , "typedef struct {float red, green, blue, alpha;} RGBA;"
  ]

hcDefines :: String
hcDefines = unlines [
    "#define HC_POINT Point"
  , "#define HC_VECTOR Vector"
  , "#define HC_PLANE Plane"
  , "#define HC_RGB RGB"
  , "#define HC_RGBA RGBA"
  ]

userIncludes :: Options -> String
userIncludes = unlines . map (\path -> "#include \"" ++ path ++ "\"") . Set.toList . oIncludes

includes :: String
includes = unlines [
    "#include \"hc.h\""
  , "#include <stdlib.h>"
  , "#include <string.h>"
  ]


envStruct :: String
envStruct = unlines [
    "#define ENV_PARAM_TYPE Env const &"
  , "struct Env {};"
  ]

globals :: String
globals = unlines [
    "extern char string_buffer[256];"
  , "extern float ff;"
  , "extern int ii;"
  , "extern long ll;"
  , "extern HC_KEY key;"
  , "extern float matrix[16];"
  ]

k :: String
k = unlines [
    "HC_KEY K(char const * seg);"
  ]

gWriteMatrix :: String
gWriteMatrix = unlines [
    "void G_Write_Matrix ("
  , "\tfloat * matrix,"
  , "\tfloat a, float b, float c, float d,"
  , "\tfloat e, float f, float g, float h,"
  , "\tfloat i, float j, float k, float l,"
  , "\tfloat m, float n, float o, float p);"
  ]

gReadFaceNormals :: String
gReadFaceNormals = unlines [
    "int G_Read_Face_Normals (HC_KEY key, char const * file);"
  ]

gReadVertexNormals :: String
gReadVertexNormals = unlines [
    "int G_Read_Vertex_Normals (HC_KEY key, char const * file);"
  ]

gReadVertexColorsByFIndex :: String
gReadVertexColorsByFIndex = unlines [
    "int G_Read_Vertex_Colors_By_FIndex (HC_KEY key, char const * file);"
  ]

gInsertMetafile :: String
gInsertMetafile = unlines [
    "HC_KEY G_Read_Metafile (char const * file);"
  ]

defineKey :: String
defineKey = unlines [
    "HC_KEY DEFINE (HC_KEY key, int tag);"
  , "HC_KEY DEFINE (HC_KEY key, int tag1, int tag2);"
  ]

lookupKey :: String
lookupKey = unlines [
    "HC_KEY LOOKUP (int tag);"
  , "HC_KEY LOOKUP (int tag1, int tag2);"
  ]

gDelete :: String
gDelete = unlines [
    "void G_Delete (char const * seg);"
  ]

gFlush :: String
gFlush = unlines [
    "void G_Flush (char const * seg);"
  ]

gMove :: String
gMove = unlines [
    "void G_Move (char const * seg, char const * newowner);"
  ]

gControlUpdate :: String
gControlUpdate = unlines [
    "void G_Control_Update (char const * seg, char const * opts);"
  ]

gIncludeSegment :: String
gIncludeSegment = unlines [
    "HC_KEY G_Include_Segment(char const * seg);"
  ]

gStyleSegment :: String
gStyleSegment = unlines [
    "HC_KEY G_Style_Segment(char const * seg);"
  ]

gConditionalStyle :: String
gConditionalStyle = unlines [
    "HC_KEY G_Conditional_Style(char const * seg, char const * cond);"
  ]

gBringToFront :: String
gBringToFront = unlines [
    "void G_Bring_To_Front (char const * seg);"
  ]

gConditionalReference :: String
gConditionalReference = unlines [
    "HC_KEY G_Conditional_Reference (char const * seg, char const * cond);"
  ]

