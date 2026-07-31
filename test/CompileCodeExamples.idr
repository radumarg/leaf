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
        Left err => pure $ Left $ "In \{programFile}: " ++ renderLexerError err
        Right tokens => pure $ Right ()
        --   case parseFile programFile tokens of
        --     Left err => pure $ Left $ "Parse error in \{programFile} at: " ++ show err.bounds ++ ", " ++ renderParseError err.val
        --     Right _ => pure $ Right ()

compileLeafFiles : String -> List String -> IO (Either String ())
compileLeafFiles examplesDirectory [] = do
  putStrLn "Finished compiling all code examples."
  pure $ Right ()
compileLeafFiles examplesDirectory (fileName :: fileNames) = do
  Right () <- compileLeafFile $ examplesDirectory ++ "/" ++ fileName
    | Left err => pure $ Left err
  compileLeafFiles examplesDirectory fileNames

export
discoverAndCompileExamples : IO (Either String ())
discoverAndCompileExamples = do
  let examplesDirectory = "examples"
  Right entries <- listDir examplesDirectory
    | Left err => pure $ Left $ "Failed to list \{examplesDirectory}: " ++ show err
  let codeExampleFiles = filter (isSuffixOf ".rs") entries
  compileLeafFiles examplesDirectory codeExampleFiles
