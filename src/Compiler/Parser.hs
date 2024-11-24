{-# LANGUAGE InstanceSigs #-}

module Compiler.Parser (parseSources, ParserCtx) where

import Compiler.Ast (TopLevelAstNode (..))
import Compiler.Sources (Source (..), SourceMap (..), Span, mkSource)
import Compiler.Tokenizer (Keyword (..), Token (..), TokenStream, TokenType (..), getNextToken, mkTokenStream)

newtype ParseError = ParseErrorKind Span deriving (Show)

newtype ParseErrorKind = ExpectedToken TokenType deriving (Show)

data ParserState = ParserState
  { parseErrors :: [ParseError],
    isInErrorState :: Bool,
    recoveryToken :: Maybe Token
  }
  deriving (Show)

data ParserCtx a = ParserCtx ParserState a deriving (Show)

instance Functor ParserCtx where
  fmap f (ParserCtx state a) = ParserCtx state (f a)

instance Applicative ParserCtx where
  pure :: a -> ParserCtx a
  pure = ParserCtx initParserState

  (<*>) :: ParserCtx (a -> b) -> ParserCtx a -> ParserCtx b
  (ParserCtx _ f) <*> (ParserCtx state2 a) = ParserCtx state2 (f a)

instance Monad ParserCtx where
  return :: a -> ParserCtx a
  return = pure

  (>>=) :: ParserCtx a -> (a -> ParserCtx b) -> ParserCtx b
  (ParserCtx _ a) >>= f =
    let (ParserCtx state2 b) = f a
     in ParserCtx state2 b

initParserState :: ParserState
initParserState = ParserState {parseErrors = [], isInErrorState = False, recoveryToken = Nothing}

parseSources :: SourceMap -> ParserCtx [TopLevelAstNode]
parseSources (SourceMap sources) =
  let state = initParserState
   in -- foldl (flip parseSource) state sources
      foldl
        (\acc source -> acc >>= \nodes -> fmap (: nodes) (parseSource source state))
        (pure [])
        sources

parseSource :: Source -> ParserState -> ParserCtx TopLevelAstNode
parseSource source state = let tokenStream = mkTokenStream source in parseTopLevelNode tokenStream state

-- parseSourceString :: String -> TopLevelAstNode
-- parseSourceString source = let tokenStream = mkTokenStream (mkSource (0, source)) in parseTopLevelNode tokenStream

parseTopLevelNode :: TokenStream -> ParserState -> ParserCtx TopLevelAstNode
parseTopLevelNode toks state =
  let tok = getNextToken toks
   in case tok of
        Just (toks', Token span (KeywordToken Fn)) -> parseFn toks' state
        Just (toks', Token span (Identifier name)) -> parseTypeDeclaration toks'
        Nothing -> error "Unexpected end of file"
        somethingElse -> error $ "Unexpected token: " ++ show somethingElse

parseFn :: TokenStream -> ParserState -> ParserCtx TopLevelAstNode
parseFn stream state =
  let result = getNextToken stream
   in case result of
        Just (stream', Token span (KeywordToken Fn)) ->
          let ident = parseIdentifier stream' state
           in case ident of
                ParserCtx state (Token span (Identifier name)) -> ParserCtx state (FunctionDeclaration name)
                _ -> error "ya5"
        Just (_, Token span _) -> error "ya2"
        Nothing -> error "ya"

parseTypeDeclaration :: TokenStream -> ParserCtx TopLevelAstNode
parseTypeDeclaration _ = undefined

parseIdentifier :: TokenStream -> ParserState -> ParserCtx Token
parseIdentifier stream state =
  let result = getNextToken stream
   in case result of
        Just (stream', Token span (Identifier name)) -> pure (Token span (Identifier name))
        Just (_, Token span _) -> error "ya3"
        Nothing -> error "ya4"