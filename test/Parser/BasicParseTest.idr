module Parser.BasicParseTest

import Text.Bounds
import Test.Simple

import Frontend.Lexer.Error
import Frontend.Lexer.Lexer
import Frontend.ASTPhases
import Frontend.Parser.Error
import Frontend.Parser.Parser
import Frontend.Syntax.AST
import Frontend.Syntax.ASTPrettyPrinter
import Frontend.Token

%default total

parseModule : String -> Maybe SurfaceSourceFile
parseModule inputProgram =
  case lexFile inputProgram of
      Left _ => Nothing
      Right tokens => case parseFile tokens of
        Left _ => Nothing
        Right ast => Just ast

prettyPrintModule : String -> Maybe String
prettyPrintModule inputProgram =
  case parseModule inputProgram of
    Nothing => Nothing
    Just sourceFile => Just (showSourceFileLax sourceFile)

export
runBasicParseTests : IO ()
runBasicParseTests = runTests $ Test.do

  test "empty input parses as an empty source file" $
    prettyPrintModule "" `shouldBe` Just ""
