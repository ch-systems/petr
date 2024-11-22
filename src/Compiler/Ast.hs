module Compiler.Ast (TopLevelAstNode (..), Module (..), TypeDeclaration) where

data TopLevelAstNode = TypeDeclaration TypeDeclaration | FunctionDeclaration String | ModuleNode Module deriving (Show)
data Module = Module {name :: String, topLevelNodes :: [TopLevelAstNode]} deriving (Show)

-- data TypeDeclaration = SumTypeDeclaration { name :: String, constructors :: List Constructor } | ProductTypeDeclaration { name :: String, fields :: List Field }
data TypeDeclaration = SumTypeDeclaration | ProductTypeDeclaration deriving (Show)
