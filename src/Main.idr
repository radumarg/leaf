module Main

import Data.String
import Text.Bounds
import System.File

import Frontend.ASTPhases
import Frontend.Token
import Frontend.Lexer.Error
import Frontend.Lexer.Lexer
import Frontend.Parser.Error
import Frontend.Parser.Parser
import Frontend.PostParseValidation
import Frontend.Source
import Frontend.Syntax.AST
import Frontend.Syntax.ASTDebugPrinter
import Frontend.Syntax.ASTPrettyPrinter

hasValidExtension : String -> Bool
hasValidExtension fileName =
  isSuffixOf ".rs" fileName ||
  isSuffixOf ".lf" fileName

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
main = do
    putStrLn "Hello from Leaf!"
    fileName <- getLine
    case hasValidExtension fileName of
      False => putStrLn "Invalid filename: expected an .rs or .lf file."
      True => do
        fileResult <- readFile fileName
        case fileResult of
          Left fileErr => putStrLn $ "Failed to read \{fileName}: " ++ show fileErr
          Right sampleProgram =>
            case lexFile sampleProgram of
              Left err => putStrLn $ renderLexerError err
              Right tokens => do
                putStrLn "Tokens:"
                traverse_ (putStrLn . show) tokens
                case parseFile fileName tokens of
                  Left err => putStrLn $ showParseError err
                  Right surfaceAST => do
                    case validateSourceFile surfaceAST of
                      errors => do
                        putStrLn "Validation errors:"
                        traverse_ (putStrLn . interpolate) errors
                      [] => do
                        putStrLn ""
                        putStrLn "AST Nodes:"
                        putStrLn $ showAstDebug surfaceAST
                        putStrLn "Pretty Printed Program:"
                        putStrLn $ "Parsed program:\n" ++ showSourceFileStrict surfaceAST
