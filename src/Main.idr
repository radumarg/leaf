module Main

import System.File
import Text.Bounds

import Frontend.ASTPhases
import Frontend.Token
import Frontend.Lexer.Error
import Frontend.Lexer.Lexer
import Frontend.Parser.Error
import Frontend.Parser.Parser
import Frontend.Syntax.AST
import Frontend.Syntax.ASTPrettyPrinter

main : IO ()
main = do
  putStrLn "Hello from Idris2!"
  fileResult <- readFile "program.rs"
  case fileResult of
    Left fileErr => putStrLn $ "Failed to read program.rs: " ++ show fileErr
    Right sampleProgram =>
      case lexFile sampleProgram of
        Left err => putStrLn $ "Lexer error: " ++ show err
        Right tokens => do
          putStrLn $ "Tokens: " ++ show tokens
          case parseFile tokens of
            Left err => putStrLn $ "Parse error at: " ++ show err.bounds ++ ", " ++ renderParseError err.val
            Right program => putStrLn $ "Parsed program:\n" ++ showSourceFileLax program
