module Parser.TopModuleParseTest

import Test.Simple

import Parser.Helper

%default total

export
runTopModuleParseTests : IO ()
runTopModuleParseTests = runTests $ Test.do

  test "empty input parses as an empty output" $
    parseAndPrettyPrint "" `shouldBe` Just ""

  test "input containing only ordinary comments parses as an empty output" $
    parseAndPrettyPrint "// nothing here\n/* still nothing */" `shouldBe`
      Just ""

  test "module declarations are not yet supported" $
    parseErrorDetails "mod my_library;" `shouldBe`
      Just ("Modules are not yet supported.", "test-fixture.rs", (1, 1), (1, 4))

  test "use statements are not yet supported" $
    parseErrorDetails "use my_library::helper;" `shouldBe`
      Just ("Use statements are not yet supported.", "test-fixture.rs", (1, 1), (1, 4))

  test "constant expression declaration" $
    parseAndPrettyPrint "const N: i64 = 4;" `shouldBe`
      Just "const N: i64 = 4;"

  test "public constant expression declaration" $
    parseAndPrettyPrint "pub const MAX_USERS: i16 = 100;" `shouldBe`
      Just "pub const MAX_USERS: i16 = 100;"

  test "constant function declaration" $
    parseAndPrettyPrint
      "const fn square(x: i64) -> i64 {\n    x * x\n}" `shouldBe`
      Just "const fn square(x: i64) -> i64 { (x * x) }"

  test "constant declarations accept array types with expression lengths" $
    parseAndPrettyPrint "const A: [i32; 2 + 2] = [1, 2, 3, 4];" `shouldBe`
      Just "const A: [i32; (2 + 2)] = [1, 2, 3, 4];"

  test "constant declarations report a missing constant name" $
    parseErrorDetails "const struct Point {}" `shouldBe`
      Just
        ( "Parse error: expected ['constant name'], but got keyword struct"
        , "test-fixture.rs"
        , (1, 7)
        , (1, 13)
        )

  test "enum declarations are not yet supported" $
    parseErrorDetails "enum Result {}" `shouldBe`
      Just ("Enums are not yet supported.", "test-fixture.rs", (1, 1), (1, 5))

  test "qenum declarations are not yet supported" $
    parseErrorDetails "qenum QResult {}" `shouldBe`
      Just ("Qenums are not yet supported.", "test-fixture.rs", (1, 1), (1, 6))

  test "struct declarations are not yet supported" $
    parseErrorDetails "struct Point {}" `shouldBe`
      Just ("Structs are not yet supported.", "test-fixture.rs", (1, 1), (1, 7))

  test "struct declarations after pub visibility are not yet supported" $
    parseErrorDetails "pub struct Point {}" `shouldBe`
      Just ("Structs are not yet supported.", "test-fixture.rs", (1, 5), (1, 11))

  test "impl blocks are not yet supported" $
    parseErrorDetails "impl Point {}" `shouldBe`
      Just ("Impl blocks and structs are not yet supported.", "test-fixture.rs", (1, 1), (1, 5))

  test "impl blocks after a function effect are not yet supported" $
    parseErrorDetails "general impl Point {}" `shouldBe`
      Just ("Impl blocks and structs are not yet supported.", "test-fixture.rs", (1, 9), (1, 13))

  test "impl blocks after const and a function effect are not yet supported" $
    parseErrorDetails "const general impl Point {}" `shouldBe`
      Just ("Impl blocks and structs are not yet supported.", "test-fixture.rs", (1, 15), (1, 19))

  test "unsupported top-level items after a function effect preserve their error" $
    parseErrorDetails "general mod my_library;" `shouldBe`
      Just ("Modules are not yet supported.", "test-fixture.rs", (1, 9), (1, 12))

  test "unsupported top-level items after const and a function effect preserve their error" $
    parseErrorDetails "const general mod my_library;" `shouldBe`
      Just ("Modules are not yet supported.", "test-fixture.rs", (1, 15), (1, 18))

  test "non-keyword items after const and a function effect report the effect" $
    parseErrorDetails "const general foo" `shouldBe`
      Just
        ( "Expected `fn` after `general` effect modifier, found instead: `foo`."
        , "test-fixture.rs"
        , (1, 15)
        , (1, 18)
        )

  test "node identifiers account for each declaration prefix modifier" $
    map parseAndListNodeIds
      [ "fn f() {}"
      , "pub fn f() {}"
      , "const fn f() {}"
      , "pub const fn f() {}"
      , "pub const unitary fn f() {}"
      , "#[qasm_gate]\nfn f() {}"
      ] `shouldBe`
      [ Just [0, 1, 3]
      , Just [0, 1, 4]
      , Just [0, 1, 4]
      , Just [0, 1, 5]
      , Just [0, 1, 6]
      , Just [0, 1, 5]
      ]

  test "outer doc comments are not yet supported" $
    parseErrorDetails "/// docs\n" `shouldBe`
      Just ("Outer doc comments are not yet supported.", "test-fixture.rs", (1, 1), (1, 9))

  test "inner doc comments are not yet supported" $
    parseErrorDetails "//! module docs" `shouldBe`
      Just ("Inner doc comments are not yet supported.", "test-fixture.rs", (1, 1), (1, 16))

  test "unexpected top-level tokens report the token" $
    parseErrorDetails "let i = 1;" `shouldBe`
      Just ( "Unexpected token: `let` at module level. Only function and constant declarations are currently supported."
           , "test-fixture.rs"
           , (1, 1)
           , (1, 4)
           )

  test "unexpected non-keyword top-level tokens report the token" $
    parseErrorDetails "foo" `shouldBe`
      Just ( "Unexpected token: `foo` at module level. Only function and constant declarations are currently supported."
           , "test-fixture.rs"
           , (1, 1)
           , (1, 4)
           )
