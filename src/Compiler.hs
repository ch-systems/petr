module Compiler (compile) where

import Compiler.Ast (TopLevelAstNode)
import Compiler.Parser (parseSources)
import Compiler.Sources (mkSourceMap)

compile :: [String] -> [TopLevelAstNode]
compile sources =
  let map = mkSourceMap sources
   in parseSources map
