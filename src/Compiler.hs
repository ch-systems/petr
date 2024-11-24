module Compiler (compile) where

import Compiler.Ast (TopLevelAstNode)
import Compiler.Parser (parseSources, ParserCtx)
import Compiler.Sources (mkSourceMap)

compile :: [String] -> ParserCtx [TopLevelAstNode]
compile sources =
  let map = mkSourceMap sources
   in parseSources map
