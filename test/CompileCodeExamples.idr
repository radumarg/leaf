module CompileCodeExamples

import Data.String
import System.Directory
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

compileLeafFile : String -> IO (Either String ())
compileLeafFile programFile = do
  fileResult <- readFile programFile
  case fileResult of
    Left fileErr => pure $ Left $ "Failed to read \{programFile}: " ++ show fileErr
    Right sampleProgram =>
      case lexFile sampleProgram of
        Left err => pure $ Left $ "Lexer error in \{programFile}: " ++ show err
        Right tokens =>
          case parseFile programFile tokens of
            Left err => pure $ Left $ "Parse error in \{programFile} at: " ++ show err.bounds ++ ", " ++ renderParseError err.val
            Right _ => pure $ Right ()

export
compileCodeExamples : IO (Either String ())
compileCodeExamples = do
  let examplesDirectory = "examples"
  Right entries <- listDir examplesDirectory
    | Left err => pure $ Left $ "Failed to list \{examplesDirectory}: " ++ show err
  let exampleFiles = filter (isSuffixOf ".rs") entries
  foldlM
    (\result, fileName =>
      case result of
        Left err => pure $ Left err
        Right () => compileLeafFile $ examplesDirectory ++ "/" ++ fileName)
    (Right ())
    exampleFiles
