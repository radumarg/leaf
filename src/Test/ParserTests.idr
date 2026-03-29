module Test.ParserTests

import Frontend.AST
import Frontend.ExprPrettyPrinter
import Frontend.Lexer
import Frontend.Parser
import Frontend.Token
import System.File
import Text.Bounds
import Tester

runTest : String -> String -> String -> {default False debug : Bool} -> IO ()
runTest fileName laxParseExpression strictParseExpression {debug} = do
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

main : IO ()
main = do
  runTest "src/Test/Good/Types/i8TypeDeclaration.lf" "let i : i8 = 1;" "let i : i8 = 1;"
  runTest "src/Test/Good/Types/i8TypeDeclaration.lf" "let i : i8 = 1;" "let i : i8 = 1;" {debug = True}
