module Run

import System

import CompileCodeExamples
import Lexer.LexerTest
import Parser.BasicParseTest

main : IO ()
main = do
  runLexerTests
  runBasicParseTests
  Right () <- compileCodeExamples
    | Left err => do
        putStrLn err
        exitFailure
  putStrLn "All tests completed."
