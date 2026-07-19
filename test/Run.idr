module Run

import System

import CompileCodeExamples
import Lexer.LexerTest
import Parser.TopModuleParseTest
import Parser.FunctionParseTest

main : IO ()
main = do
  runLexerTests
  runTopModuleParseTests
  runFunctionParseTests
  Right () <- discoverAndCompileExamples
    | Left err => do
        putStrLn err
        exitFailure
  putStrLn "All tests completed."
