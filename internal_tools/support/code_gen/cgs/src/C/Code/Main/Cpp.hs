module C.Code.Main.Cpp (
  main_cpp
) where

--------------------------------------------------------------------------------

main_cpp :: Int -> String
main_cpp numFiles = unlines [
    "#include \"shared.h\""
  , "#include \"hversion.h\""
  , ""
  ] ++ concatMap extern fileNums
  ++ unlines [
    ""
  , "int main () {"
  , "\tHC_Define_System_Options(INTERNAL_RELEASE_LICENSE);"
  , "\tHC_Define_System_Options(\"errors = (disable = (42/143))\"); // metafile version"
  , "\tHC_Define_System_Options(\"warnings = (disable = (8/413))\"); // update one display on not isolated"
  , ""
  , "\tEnv env;"
  , ""
  ] ++ concatMap runFile fileNums
  ++ unlines [
    ""
  , "\treturn 0;"
  , "}"
  , ""
  ]
  where
    fileNums = [0..numFiles - 1]

extern :: Int -> String
extern n = unlines [
    "extern int code_chain_" ++ show n ++ " (ENV_PARAM_TYPE);"
  ]

runFile :: Int -> String
runFile n = unlines $ map ("\t" ++) [
    "code_chain_" ++ show n ++ "(env);"
  ]




