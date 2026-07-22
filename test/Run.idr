module Run

import System

import CompileCodeExamples
import Lexer.LexerTest
import Parser.ExpressionParseTest
import Parser.FunctionParseTest
import Parser.TopModuleParseTest

main : IO ()
main = do
  runLexerTests
  runExpressionParseTests
  runFunctionParseTests
  runTopModuleParseTests
  Right () <- discoverAndCompileExamples
    | Left err => do
        putStrLn err
        exitFailure
  putStrLn "All tests completed."
