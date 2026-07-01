module Test.TestsHelper

import Frontend.AST
import Frontend.ExprPrettyPrinter
import Frontend.Lexer
import Frontend.Parser
import Frontend.Token
import System.File
import Text.Bounds
import Tester

export
runParseOkTest : String -> String -> String -> {default False debug : Bool} -> IO ()
runParseOkTest fileName laxParseExpression strictParseExpression {debug} = do
  fileResult <- readFile fileName
  case fileResult of
    Left fileErr => putStrLn $ "Failed to read " ++ fileName ++ ": " ++ show fileErr
    Right testProgram =>
      case lexProgram testProgram of
        Left err => putStrLn $ "Lexer error in " ++ fileName ++ ": " ++ show err
        Right tokens =>
          case parseProgramAll tokens of
            Left err => putStrLn $ "Parse error in " ++ fileName ++ ": " ++ show err
            Right program => do
              let laxParseResult = showProgramLax program
              let strictParseResult = showProgramStrict program
              case debug of
                True => do
                  putStrLn $ "Parsed program: " ++ show program
                  putStrLn $ "Lax parsed expression: " ++ laxParseResult
                  putStrLn $ "Strict parsed expression: " ++ strictParseResult
                False => do
                  result <- runEitherT $ do
                    assertEq laxParseExpression laxParseResult
                  case result of
                    Left err => putStrLn ("Lax parsing: " ++ err ++ " in " ++ fileName)
                    Right () => pure ()
                  result <- runEitherT $ do
                    assertEq strictParseExpression strictParseResult
                  case result of
                    Left err => putStrLn ("Strict parsing: " ++ err ++ " in " ++ fileName)
                    Right () => pure ()

export
runParseShouldFailTest : String -> {default False debug : Bool} -> IO ()
runParseShouldFailTest fileName {debug} = do
  fileResult <- readFile fileName
  case fileResult of
    Left fileErr => putStrLn $ "Failed to read " ++ fileName ++ ": " ++ show fileErr
    Right testProgram =>
      case lexProgram testProgram of
        Left err =>
          case debug of
            True => putStrLn $ "Expected lexer failure in " ++ fileName ++ ": " ++ show err
            False => pure ()
        Right tokens =>
          case parseProgramAll tokens of
            Left err =>
              case debug of
                True => putStrLn $ "Expected parse failure in " ++ fileName ++ ": " ++ show err
                False => pure ()
            Right program => do
              let laxParseResult = showProgramLax program
              let strictParseResult = showProgramStrict program
              putStrLn $ "Expected invalid type declaration to fail in " ++ fileName
              putStrLn $ "Lax parsed expression: " ++ laxParseResult
              putStrLn $ "Strict parsed expression: " ++ strictParseResult