module Parser.FunctionParseTest

import Test.Simple

import Parser.Helper

%default total

export
runFunctionParseTests : IO ()
runFunctionParseTests = runTests $ Test.do

  test "empty function" $
    parseAndPrettyPrint "fn empty() {}" `shouldBe` Just "fn empty() { }"

  test "pub empty function" $
    parseAndPrettyPrint "pub fn empty() {}" `shouldBe` Just "pub fn empty() { }"

  test "empty function with unit output" $
    parseAndPrettyPrint "fn empty() -> () {}" `shouldBe` Just "fn empty() -> () { }"

  test "unitary empty function with unit output" $
    parseAndPrettyPrint "unitary fn empty() -> () {}" `shouldBe` Just "unitary fn empty() -> () { }"

  test "pub unitary empty function with unit output" $
    parseAndPrettyPrint "pub unitary fn empty() -> () {}" `shouldBe` Just "pub unitary fn empty() -> () { }"

  test "empty function with unit output and simple annotation" $
    parseAndPrettyPrint "#[qasm_gate]\nfn empty() -> () {}" `shouldBe` Just "#[qasm_gate]\nfn empty() -> () { }"

  test "empty general pub function with unit output and annotation with text" $
    parseAndPrettyPrint  "#[qasm_def(\"qasm_subroutine_name\")]\npub general fn empty() -> () {}" 
      `shouldBe` Just "#[qasm_def(\"qasm_subroutine_name\")]\npub general fn empty() -> () { }"

  test "function with two annotations" $
    parseAndPrettyPrint  "#[qasm_gate]\n#[qasm_def(\"qasm_subroutine_name\")]\npub general fn empty() -> () {}" 
      `shouldBe` Just "#[qasm_gate]\n#[qasm_def(\"qasm_subroutine_name\")]\npub general fn empty() -> () { }"

  test "empty function with parameters" $
    parseAndPrettyPrint "fn add(i : i32, point : (i32, i32)) {}" `shouldBe` Just "fn add(i: i32, point: (i32, i32)) { }"

  test "function with a simple statement" $
    parseAndPrettyPrint "fn simple() {let i: i32 = 1;}" `shouldBe` Just "fn simple() { let i: i32 = 1; }"

  test "function with a simple return expression" $
    parseAndPrettyPrint "fn simple_expression() -> i64 {1}" `shouldBe` Just "fn simple_expression() -> i64 { 1 }"

  test "annotation applied to a statement instead to a function declaration" $
    parseErrorDetails "#[qasm_gate]\nlet i = 1;" `shouldBe`
      Just ("Expected function declaration after attribute, found instead: `let`.", "test-fixture.rs", (2, 1), (2, 4))

  test "function expected after pub visibility modifier" $
    parseErrorDetails "pub let i = 1;" `shouldBe`
      Just ("Expected function declaration after `pub` visibility modifier, found instead: `let`.", "test-fixture.rs", (1, 5), (1, 8))

  test "function keyword expected after function effect" $
    parseErrorDetails "pub general let i = 1;" `shouldBe`
      Just ("Expected `fun` after `general` effect modifier, found instead: `let`.", "test-fixture.rs", (1, 13), (1, 16))

  test "malformed attribute, missing closing bracket" $
    parseErrorDetails "#[qasm_gate \nfn empty() -> () {}" `shouldBe`
      Just ("Malformed attribute.", "test-fixture.rs", (1, 1), (1, 2))

  test "malformed attribute, extra content" $
    parseErrorDetails "#[qasm_gate bogus_name] \nfn empty() -> () {}" `shouldBe`
      Just ("Malformed attribute.", "test-fixture.rs", (1, 1), (1, 2))

  test "malformed attribute with argument, missing closing bracket" $
    parseErrorDetails "#[qasm_def(\"qasm_subroutine_name\") \nfn empty() -> () {}" `shouldBe`
      Just ("Malformed attribute.", "test-fixture.rs", (1, 1), (1, 2))

  test "malformed attribute with argument, extra content" $
    parseErrorDetails "#[qasm_def(\"qasm_subroutine_name\") bogus_name] \nfn empty() -> () {}" `shouldBe`
      Just ("Malformed attribute.", "test-fixture.rs", (1, 1), (1, 2))

  test "function without opening brace" $
    parseErrorDetails "fn empty(); }" `shouldBe`
      Just ("Expected a function body declaration starting with `{`, found instead: `;`.", "test-fixture.rs", (1, 11), (1, 12))

  -- test "function with statement missing semicolon" $
  --   parseErrorDetails "fn simple() { let i: i32 = 1 }" `shouldBe`
  --     Just ("Expected a function body declaration starting with `{`, found instead: `;`.", "test-fixture.rs", (1, 11), (1, 12))

  -- debugTestParseError "fn simple() { let i: i32 = 1;"
