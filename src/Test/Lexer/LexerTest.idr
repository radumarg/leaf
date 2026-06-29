module Test.Lexer.LexerTest

import Text.Bounds
import Test.Simple

import Frontend.Token
import Frontend.Lexer.Errors
import Frontend.Lexer.Lexer

%default total

tokenValues : Either (Bounded LexerError) (List (Bounded Token)) -> Either LexerError (List Token)
tokenValues (Left boundedError) =
  Left boundedError.val
tokenValues (Right boundedTokens) =
  Right (map val boundedTokens)

lexTokenValues : String -> Either LexerError (List Token)
lexTokenValues input =
  tokenValues (lexProgram input)

export
runLexerTests : IO ()
runLexerTests = runTests $ Test.do
  test "keyword fn" $
    lexTokenValues "fn" `shouldBe` Right [TokKw KwFn, TokEOF]

  test "identifier" $
    lexTokenValues "hello" `shouldBe` Right [TokIdent "hello", TokEOF]

  test "underscore" $
    lexTokenValues "_" `shouldBe` Right [TokUnderscore, TokEOF]

  test "apostrophe keeps keyword-looking identifier ordinary" $
    lexTokenValues "fn'" `shouldBe` Right [TokIdent "fn'", TokEOF]

  test "unary minus stays separate from integer literal" $
    lexTokenValues "-7" `shouldBe` Right [TokSym SymMinus, TokIntLitRaw "7", TokEOF]

  test "suffixed integer-looking float is a float" $
    lexTokenValues "5f32" `shouldBe` Right [TokFloatLitRaw "5f32", TokEOF]

  test "intrinsics are builtins but prelude names are identifiers" $
    lexTokenValues "qalloc Param phase turns" `shouldBe`
      Right
        [ TokBuiltin BuiltinQAlloc
        , TokIdent "Param"
        , TokIdent "phase"
        , TokIdent "turns"
        , TokEOF
        ]

  test "normal comments are skipped" $
    lexTokenValues "let /* nested /* comment */ ok */ x" `shouldBe`
      Right [TokKw KwLet, TokIdent "x", TokEOF]

  test "outer line doc is preserved" $
    lexTokenValues "/// docs\nfn" `shouldBe`
      Right [TokOuterDoc "/// docs", TokKw KwFn, TokEOF]

  test "four slash line comment is normal comment" $
    lexTokenValues "//// docs\nfn" `shouldBe` Right [TokKw KwFn, TokEOF]

  test "inner block doc can be empty" $
    lexTokenValues "/*!*/fn" `shouldBe`
      Right [TokInnerDoc "/*!*/", TokKw KwFn, TokEOF]

  test "malformed radix number is a lexer error" $
    lexTokenValues "0b102" `shouldBe` Left (LexInvalidNumberLiteral "0b102")

  test "unterminated block comment is a lexer error" $
    lexTokenValues "/* nope" `shouldBe` Left LexUnterminatedBlockComment
