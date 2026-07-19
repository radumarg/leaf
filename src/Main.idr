module Main

import System.File
import Text.Bounds

import Frontend.ASTPhases
import Frontend.Token
import Frontend.Lexer.Error
import Frontend.Lexer.Lexer
import Frontend.Parser.Error
import Frontend.Parser.Parser
import Frontend.Source
import Frontend.Syntax.AST
import Frontend.Syntax.ASTPrettyPrinter

showParseError : Located ParseError -> String
showParseError err =
  let errorFile = err.span.file
      errorSpanStart = err.span.start
  in "Parse error at: "
      ++ errorFile
      ++ ", at line: "
      ++ show errorSpanStart.line
      ++ ", at column: "
      ++ show errorSpanStart.column
      ++ ", with message: "
      ++ renderParseError err.value

main : IO ()
main = let programFile = "program.rs" in
  do
    putStrLn "Hello from Leaf!"
    fileResult <- readFile programFile
    case fileResult of
      Left fileErr => putStrLn $ "Failed to read \{programFile}: " ++ show fileErr
      Right sampleProgram =>
        case lexFile sampleProgram of
          Left err => putStrLn $ "Lexer error: " ++ show err
          Right tokens => do
            case parseFile programFile tokens of
              Left err => putStrLn $ showParseError err
              Right program => putStrLn $ "Parsed program:\n" ++ showSourceFileLax program
