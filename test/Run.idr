module Run

import System

import CompileCodeExamples
import CompileCodeExamplesTest
import Desugarer.DesugaringTest
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
  runDesugaringTests
  runCompileCodeExamplesTests
  Right () <- discoverAndCompileExamples
    | Left err => do
        putStrLn err
        exitFailure
  putStrLn "All tests completed."
