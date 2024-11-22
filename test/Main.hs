module Main (main) where

import Compiler (compile)
import Test.HUnit

main :: IO ()
main = undefined

testSource = "fn something (a in 'int, b in 'int) returns 'nothing ()"
test1 =
  let result = compile [testSource]
   in assertEqual "for the length of the result," 1 (length result)
