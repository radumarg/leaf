module Parser.TopModuleParseTest

import Test.Simple

import Parser.Helper

%default total

export
runTopModuleParseTests : IO ()
runTopModuleParseTests = runTests $ Test.do

  test "empty input parses as an empty output" $
    parseAndPrettyPrint "" `shouldBe` Just ""

  test "module declarations are not yet supported" $
    parseErrorDetails "mod my_library;" `shouldBe`
      Just ("Modules are not yet supported.", "test-fixture.rs", (1, 1), (1, 3))

  test "use statements are not yet supported" $
    parseErrorDetails "use my_library::helper;" `shouldBe`
      Just ("Use statements are not yet supported.", "test-fixture.rs", (1, 1), (1, 3))

  test "const declarations are not yet supported" $
    parseErrorDetails "const VALUE: i32 = 1;" `shouldBe`
      Just ("Const declarations or const functions are not yet supported.", "test-fixture.rs", (1, 1), (1, 5))

  test "enum declarations are not yet supported" $
    parseErrorDetails "enum Result {}" `shouldBe`
      Just ("Enums are not yet supported.", "test-fixture.rs", (1, 1), (1, 4))

  test "qenum declarations are not yet supported" $
    parseErrorDetails "qenum QResult {}" `shouldBe`
      Just ("Qenums are not yet supported.", "test-fixture.rs", (1, 1), (1, 5))

  test "struct declarations are not yet supported" $
    parseErrorDetails "struct Point {}" `shouldBe`
      Just ("Structs are not yet supported.", "test-fixture.rs", (1, 1), (1, 6))

  test "impl blocks are not yet supported" $
    parseErrorDetails "impl Point {}" `shouldBe`
      Just ("Impls blocks and structs are not yet supported.", "test-fixture.rs", (1, 1), (1, 4))

  test "outer doc comments are not yet supported" $
    parseErrorDetails "/// docs\n" `shouldBe`
      Just ("Outer doc comments are not yet supported.", "test-fixture.rs", (1, 1), (1, 8))

  test "unexpected top-level tokens report the token" $
    parseErrorDetails "let i = 1;" `shouldBe`
      Just ( "Unexpected token: `let` at top level in source file. At module level only only function declarations are allowed for now."
           , "test-fixture.rs"
           , (1, 1)
           , (1, 3)
           )
