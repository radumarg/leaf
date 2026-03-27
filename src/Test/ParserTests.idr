module Test.ParserTests

import Frontend.AST
import Frontend.ExprPrettyPrinter
import Frontend.Lexer
import Frontend.Parser
import Frontend.Token
import System.File
import Text.Bounds

runTest : String -> IO ()
runTest fileName = do
  fileResult <- readFile fileName
  case fileResult of
    Left fileErr => putStrLn $ "Failed to read " ++ fileName ++ ": " ++ show fileErr
    Right testProgram =>
      case lexProgram testProgram of
        Left err => putStrLn $ "Lexer error: " ++ show err
        Right tokens =>
          case parseProgramAll tokens of
            Left err => putStrLn $ "Parse error: " ++ show err
            Right program => putStrLn $ "Parsed program: " ++ show program

main : IO ()
main = do
  runTest("src/Test/Good/Types/i8TypeDeclaration.lf")