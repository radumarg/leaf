module Test.ParserTests

import Frontend.AST
import Frontend.ExprPrettyPrinter
import Frontend.Lexer
import Frontend.Parser
import Frontend.Token
import System.File
import Text.Bounds
import Tester
import Tester.Runner

runTest : String -> String -> {default False debug : Bool} -> IO ()
runTest fileName parseExpression {debug} = do
  fileResult <- readFile fileName
  case fileResult of
    Left fileErr => putStrLn $ "Failed to read " ++ fileName ++ ": " ++ show fileErr
    Right testProgram =>
      case lexProgram testProgram of
        Left err => putStrLn $ "Lexer error: " ++ show err
        Right tokens =>
          case parseProgramAll tokens of
            Left err => putStrLn $ "Parse error: " ++ show err
            Right program => do
              let strictParseResult = showProgramStrict program
              case debug of
                True => do
                  putStrLn $ "Parsed program: " ++ show program
                  putStrLn $ "Strict parse expression: " ++ strictParseResult
                False => do
                  result <- runEitherT $ assertEq parseExpression strictParseResult
                  case result of
                    Left err => putStrLn err
                    Right () => pure ()

main : IO ()
main = do
  runTest "src/Test/Good/Types/i8TypeDeclaration.lf" "let i : i8 = 1;"
  runTest "src/Test/Good/Types/i8TypeDeclaration.lf" "let i : i8 = 1;" {debug = True}
