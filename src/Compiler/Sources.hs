module Compiler.Sources (SourceId, SourceMap (..), Source (..), Span (..), mkSource, mkSourceMap) where

newtype SourceId = SourceId Int deriving (Show)

-- an IndexMap
newtype SourceMap = SourceMap [Source]
data Source = Source SourceId String
data Span = Span Int Int SourceId deriving (Show)

mkSourceMap :: [String] -> SourceMap
mkSourceMap sources =
  let sources_with_indices = zip [0 ..] sources
   in SourceMap $ map mkSource sources_with_indices

mkSource :: (Int, String) -> Source
mkSource (index, source) = Source (SourceId index) source
