module Parser.Helper

import Text.Bounds
import Test.Simple

import Frontend.Lexer.Error
import Frontend.Lexer.Lexer
import Frontend.ASTPhases
import Frontend.Parser.Error
import Frontend.Parser.Parser
import Frontend.Source
import Frontend.Syntax.AST
import Frontend.Syntax.ASTPrettyPrinter
import Frontend.Token

%default total

lexThenParse : String -> String -> Maybe SurfaceSourceFile
lexThenParse fileName inputProgram =
  case lexFile inputProgram of
      Left _ => Nothing
      Right tokens => case parseFile fileName tokens of
        Left _ => Nothing
        Right ast => Just ast

export
parseAndPrettyPrint : String -> Maybe String
parseAndPrettyPrint inputProgram =
  case lexThenParse "test-fixture.rs" inputProgram of
    Nothing => Nothing
    Just sourceFile => Just (showSourceFileStrict sourceFile)

export
parseErrorDetails : String -> Maybe (String, String, (Nat, Nat), (Nat, Nat))
parseErrorDetails inputProgram =
  case lexFile inputProgram of
    Left _ => Nothing
    Right tokens =>
      case parseFile "test-fixture.rs" tokens of
        Left located =>
          let errorSpan = located.span
              errorStart = errorSpan.start
              errorEnd = errorSpan.end
          in Just (renderParseError located.value,
                                    errorSpan.file,
                                    (errorStart.line, errorStart.column),
                                    (errorEnd.line, errorEnd.column))
        Right _ => Nothing