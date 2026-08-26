module Run

import System

import CompileCodeExamples
import CompileCodeExamplesTest
import Lexer.LexerTest
import PostParseValidator.PostParseValidationTest
import Parser.ExpressionParseTest
import Parser.FunctionParseTest
import Parser.TopModuleParseTest

main : IO ()
main = do
  runLexerTests
  runExpressionParseTests
  runFunctionParseTests
  runTopModuleParseTests
  runPostParseValidationTests
  runCompileCodeExamplesTests
  Right () <- discoverAndCompileExamples
    | Left err => do
        putStrLn err
        exitFailure
  putStrLn "All tests completed."
