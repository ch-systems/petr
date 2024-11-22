module Compiler.Parser (parseSources) where

import Compiler.Ast (TopLevelAstNode)
import Compiler.Sources (Source (..), SourceMap (..), mkSource)
import Compiler.Tokenizer (Keyword (..), Token (..), TokenStream, TokenType (..), getNextToken, mkTokenStream)

parseSources :: SourceMap -> [TopLevelAstNode]
parseSources (SourceMap sources) = map parseSource sources

parseSource :: Source -> TopLevelAstNode
parseSource source = let tokenStream = mkTokenStream source in parseTopLevelNode tokenStream

parseSourceString :: String -> TopLevelAstNode
parseSourceString source = let tokenStream = mkTokenStream (mkSource (0, source)) in parseTopLevelNode tokenStream

parseTopLevelNode :: TokenStream -> TopLevelAstNode
parseTopLevelNode toks =
  let tok = getNextToken toks
   in case tok of
        Just (toks', Token span (KeywordToken Fn)) -> parseFn toks'
        Just (toks', Token span (Identifier name)) -> parseTypeDeclaration toks'
        Nothing -> error "Unexpected end of file"
        somethingElse -> error $ "Unexpected token: " ++ show somethingElse

parseFn :: TokenStream -> TopLevelAstNode
parseFn _ = undefined

parseTypeDeclaration :: TokenStream -> TopLevelAstNode
parseTypeDeclaration _ = undefined
