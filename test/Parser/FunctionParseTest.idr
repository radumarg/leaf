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

  -- test "annotation applied to a statement instead to a function declaration" $
  --   parseErrorDetails "#[qasm_gate]\nlet 1 = 1;" `shouldBe`
  --     Just ("?", "test-fixture.rs", (1, 1), (1, 4))

  -- debugTestParseError "#[qasm_gate]\nlet i = 1;"

