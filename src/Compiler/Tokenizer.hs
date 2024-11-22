module Compiler.Tokenizer (Token (..), TokenType (..), Keyword (..), TokenStream (..), mkTokenStream, getNextToken) where

import Compiler.Sources (Source (..), SourceId (..), Span (..))

-- Tokenizer
data Token = Token Span TokenType deriving (Show)
data TokenType = Identifier String | KeywordToken Keyword | Operator | Literal | CommentToken deriving (Show)
data Keyword = Fn deriving (Show)

-- mkLazyTokenStream :: Source -> [Token]
-- mkLazyTokenStream (Source index source) = map mkToken (zip [0..] source)

-- Given a string, return a list of spanned strings where the span represents the start and end of each whitespace-separated word.
-- e.g. "hello world" -> [(Span 0 5, "Hello"), (Span 6 11, "world)]
-- mkSpannedWords :: String -> [(Span, String)]
mkSpannedWords sourceId source = mkSpannedWords' sourceId 0 "" source

data TokenStream = TokenStream SourceId [Char] deriving (Show)

mkTokenStream :: Source -> TokenStream
mkTokenStream (Source sourceId source) = TokenStream sourceId source

getNextToken :: TokenStream -> Maybe (TokenStream, Token)
getNextToken (TokenStream _ []) = Nothing
getNextToken (TokenStream sourceId source) =
  let (span, nextWord, rest) = mkSpannedWords sourceId source
      token_type = tokenTypeFromWord nextWord
      token = Token span token_type
   in Just (TokenStream sourceId rest, token)

tokenTypeFromWord :: String -> TokenType
tokenTypeFromWord "fn" = KeywordToken Fn
tokenTypeFromWord x = Identifier x

mkSpannedWords' :: SourceId -> Int -> [Char] -> [Char] -> (Span, [Char], [Char])
mkSpannedWords' sourceId ixThusFar wordBuf [] =
  let lo = ixThusFar - length wordBuf
      hi = ixThusFar
      span = Span lo hi sourceId
   in (span, wordBuf, [])
mkSpannedWords' sourceId ixThusFar wordBuf (' ' : rest) =
  let lo = ixThusFar - length wordBuf
      hi = ixThusFar
      span = Span lo hi sourceId
   in (span, wordBuf, rest)
mkSpannedWords' sourceId ixThusFar wordBuf (x : xs) = mkSpannedWords' sourceId (ixThusFar + 1) (wordBuf ++ [x]) xs
