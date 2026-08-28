module Main

import Data.String
import Text.Bounds
import System.File

import Compiler.Desugar.Desugar
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

isOneWord : String -> Bool
isOneWord fileName =
  fileName /= "" &&
  not (any isSpace (unpack fileName))

isValidLeafFileName : String -> Bool
isValidLeafFileName fileName =
  isOneWord fileName &&
  (isSuffixOf ".rs" fileName || isSuffixOf ".lf" fileName)

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

showAstAndProgram : PhasePretty phase => SourceFile phase -> IO ()
showAstAndProgram astTree = do
  putStrLn "AST Nodes:"
  putStrLn $ showAstDebug astTree
  putStrLn "Pretty Printed Program:"
  putStrLn $ "Parsed program:\n" ++ showSourceFileStrict astTree

main : IO ()
main = do
    putStrLn "Hello from Leaf!"
    fileName <- getLine
    case isValidLeafFileName fileName of
      False => putStrLn "Invalid filename: expected an .rs or .lf file."
      True => do
        fileResult <- readFile fileName
        case fileResult of
          Left fileErr => putStrLn $ "Failed to read \{fileName}: " ++ show fileErr
          Right sampleProgram =>
            case lexFile sampleProgram of
              Left err => putStrLn $ renderLexerError err
              Right tokens => do
                case parseFile fileName tokens of
                  Left err => putStrLn $ showParseError err
                  Right surfaceAst => do
                    case validateSourceFile surfaceAst of
                      [] => do
                          let canonicalAst = desugarSurfaceSyntax surfaceAst
                          showAstAndProgram canonicalAst
                      errors => do
                        putStrLn "Validation errors:"
                        traverse_ (putStrLn . interpolate) errors


