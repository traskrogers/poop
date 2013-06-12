module C.Code.Shared.Cpp (
  shared_cpp
) where

--------------------------------------------------------------------------------

shared_cpp :: String
shared_cpp = unlines [
    includes
  , globals
  , k
  , anonNamespace hxAssociate
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

anonNamespace :: String -> String
anonNamespace = unlines . wrap . map ('\t':) . lines
  where
    wrap ls = "namespace {" : ls ++ ["}"]

includes :: String
includes = unlines [
    "#include \"shared.h\""
  , "#include \"hc.h\""
  , "#include <assert.h>"
  , "#include <stdlib.h>"
  , "#include <string>"
  , "#include <string.h>"
  , "#include <map>"
  , "#include <utility>"
  , "#include <iostream>"
  , "#include <fstream>"
  ]

k :: String
k = unlines [
    "HC_KEY K (char const * seg) {"
  , "\treturn HC_Create_Segment(seg);"
  , "}"
  ]

globals :: String
globals = unlines [
    "char string_buffer[256];"
  , "float ff;"
  , "int ii;"
  , "long ll;"
  , "HC_KEY key;"
  , "float matrix[16];"
  ]

gWriteMatrix :: String
gWriteMatrix = unlines [
    "void G_Write_Matrix ("
  , "\tfloat * matrix,"
  , "\tfloat a, float b, float c, float d,"
  , "\tfloat e, float f, float g, float h,"
  , "\tfloat i, float j, float k, float l,"
  , "\tfloat m, float n, float o, float p)"
  , "{"
  , "\tfloat tmp[] = {a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p};"
  , "\tmemcpy(matrix, tmp, sizeof(tmp));"
  , "}"
  ]

gReadFaceNormals :: String
gReadFaceNormals = unlines [
    "int G_Read_Face_Normals (HC_KEY key, char const * file) {"
  , "\tstd::ifstream is(file);"
  , "\tif (!is) {"
  , "\t\tstd::cout << \"Failed to open \" << file << std::endl;"
  , "\t\treturn 1;"
  , "\t}"
  , "\tint offset;"
  , "\tis >> offset;"
  , "\tint size;"
  , "\tis >> size;"
  , "\tfloat * normals = new float[3 * size];"
  , "\tfor (int i = 0; i < 3 * size; ++i) {"
  , "\t\tis >> normals[i];"
  , "\t}"
  , "\tHC_MSet_Face_Normals(key, offset, size, (HC_VECTOR const *) normals);"
  , "\tdelete [] normals;"
  , "\tis.close();"
  , "\treturn 0;"
  , "}"
  ]

gReadVertexNormals :: String
gReadVertexNormals = unlines [
    "int G_Read_Vertex_Normals (HC_KEY key, char const * file) {"
  , "\tstd::ifstream is(file);"
  , "\tif (!is) {"
  , "\t\tstd::cout << \"Failed to open \" << file << std::endl;"
  , "\t\treturn 1;"
  , "\t}"
  , "\tint offset;"
  , "\tis >> offset;"
  , "\tint size;"
  , "\tis >> size;"
  , "\tfloat * normals = new float[3 * size];"
  , "\tfor (int i = 0; i < 3 * size; ++i) {"
  , "\t\tis >> normals[i];"
  , "\t}"
  , "\tHC_MSet_Vertex_Normals(key, offset, size, (HC_VECTOR const *) normals);"
  , "\tdelete [] normals;"
  , "\tis.close();"
  , "\treturn 0;"
  , "}"
  ]

gReadVertexColorsByFIndex :: String
gReadVertexColorsByFIndex = unlines [
    "int G_Read_Vertex_Colors_By_FIndex (HC_KEY key, char const * file) {"
  , "\tstd::ifstream is(file);"
  , "\tif (!is) {"
  , "\t\tstd::cout << \"Failed to open \" << file << std::endl;"
  , "\t\treturn 1;"
  , "\t}"
  , "\tstd::string types;"
  , "\tstd::getline(is, types);"
  , "\tint offset;"
  , "\tis >> offset;"
  , "\tint size;"
  , "\tis >> size;"
  , "\tfloat * findices = new float[size];"
  , "\tfor (int i = 0; i < size; ++i) {"
  , "\t\tis >> findices[i];"
  , "\t}"
  , "\tHC_MSet_Vertex_Colors_By_FIndex(key, types.c_str(), offset, size, findices);"
  , "\tdelete [] findices;"
  , "\tis.close();"
  , "\treturn 0;"
  , "}"
  ]

gInsertMetafile :: String
gInsertMetafile = unlines [
    "HC_KEY G_Read_Metafile (char const * file) {"
  , "\tHC_KEY key;"
  , "\tHC_KEY tempKey = HC_Open_Segment(\"\");{"
  , "\t\tHC_Read_Metafile(file, \".\", \"\");"
  , "\t\tHC_Begin_Contents_Search(\".\", \"everything\");{"
  , "\t\t\tint count;"
  , "\t\t\tHC_Show_Contents_Count(&count);"
  , "\t\t\tif (count != 1) {"
  , "\t\t\t\treturn -1;"
  , "\t\t\t}"
  , "\t\t\tHC_Find_Contents(0, &key);"
  , "\t\t}HC_End_Contents_Search();"
  , "\t}HC_Close_Segment();"
  , "\tHC_Move_By_Key(key, \".\");"
  , "\tHC_Delete_By_Key(tempKey);"
  , "\treturn key;"
  , "}"
  ]

hxAssociate :: String
hxAssociate = unlines [
    "typedef std::pair<int, int> Tag;"
  , "typedef std::map<Tag, HC_KEY> KeyMap;"
  , ""
  , "KeyMap keyMap;"
  , ""
  , "bool associateCalled = false;"
  , "int associateTagSize;"
  , ""
  , "HC_KEY associate (HC_KEY key, bool insert, Tag tag, int tagSize) {"
  , "\tif (!associateCalled) {"
  , "\t\tassociateTagSize = tagSize;"
  , "\t\tassociateCalled = true;"
  , "\t}"
  , "\tif (associateTagSize != tagSize) {"
  , "\t\tassert(false); // need to be consistent with DEFINE and LOOKUP key sizes"
  , "\t}"
  , "\tif (insert) {"
  , "\t\treturn keyMap[tag] = key;"
  , "\t}"
  , "\telse {"
  , "\t\tKeyMap::iterator iter = keyMap.find(tag);"
  , "\t\tif (iter == keyMap.end()) {"
  , "\t\t\treturn -1;"
  , "\t\t}"
  , "\t\treturn iter->second;"
  , "\t}"
  , "}"
  ]

defineKey :: String
defineKey = unlines [
    "HC_KEY DEFINE (HC_KEY key, int tag) {"
  , "\treturn associate(key, true, Tag(tag, -1), 1);"
  , "}"
  , ""
  , "HC_KEY DEFINE (HC_KEY key, int tag1, int tag2) {"
  , "\treturn associate(key, true, Tag(tag1, tag2), 2);"
  , "}"
  ]

lookupKey :: String
lookupKey = unlines [
    "HC_KEY LOOKUP (int tag) {"
  , "\treturn associate(0, false, Tag(tag, -1), 1);"
  , "}"
  , ""
  , "HC_KEY LOOKUP (int tag1, int tag2) {"
  , "\treturn associate(0, false, Tag(tag1, tag2), 2);"
  , "}"
  ]

gDelete :: String
gDelete = unlines [
    "void G_Delete (char const * seg) {"
  , "\tHC_Create_Segment(seg);"
  , "\tHC_Delete_Segment(seg);"
  , "}"
  ]

gFlush :: String
gFlush = unlines [
    "void G_Flush (char const * seg) {"
  , "\tHC_KEY key = HC_Create_Segment(seg);"
  , "\tHC_Flush_By_Key(key);"
  , "}"
  ]

gMove :: String
gMove = unlines [
    "void G_Move (char const * seg, char const * newowner) {"
  , "\tHC_KEY key = HC_Create_Segment(seg);"
  , "\tHC_Move_By_Key(key, newowner);"
  , "}"
  ]

gControlUpdate :: String
gControlUpdate = unlines [
    "void G_Control_Update (char const * seg, char const * opts) {"
  , "\tHC_Create_Segment(seg);"
  , "\tHC_Control_Update(seg, opts);"
  , "}"
  ]

gIncludeSegment :: String
gIncludeSegment = unlines [
    "HC_KEY G_Include_Segment (char const * seg) {"
  , "\tHC_Create_Segment(seg);"
  , "\treturn HC_Include_Segment(seg);"
  , "}"
  ]

gStyleSegment :: String
gStyleSegment = unlines [
    "HC_KEY G_Style_Segment (char const * seg) {"
  , "\tHC_Create_Segment(seg);"
  , "\treturn HC_Style_Segment(seg);"
  , "}"
  ]

gConditionalStyle :: String
gConditionalStyle = unlines [
    "HC_KEY G_Conditional_Style (char const * seg, char const * cond) {"
  , "\tHC_Create_Segment(seg);"
  , "\treturn HC_Conditional_Style(seg, cond);"
  , "}"
  ]

gBringToFront :: String
gBringToFront = unlines [
    "void G_Bring_To_Front (char const * seg) {"
  , "\tHC_Create_Segment(seg);"
  , "\tHC_Bring_To_Front(seg);"
  , "}"
  ]

gConditionalReference :: String
gConditionalReference = unlines [
    "HC_KEY G_Conditional_Reference (char const * seg, char const * cond) {"
  , "\tHC_KEY key = HC_Create_Segment(seg);"
	, "\treturn HC_Conditional_Reference_By_Key(key, cond);"
  , "}"
  ]

