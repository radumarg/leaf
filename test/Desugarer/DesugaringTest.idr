module Desugarer.DesugaringTest

import Test.Simple

import Desugarer.Helper

%default total

export
runDesugaringTests : IO ()
runDesugaringTests = runTests $ Test.do

  test "missing return type is filled in with -> ()" $
    desugarAndPrettyPrint "general fn empty() {}"
      `shouldBe` Just "general fn empty() -> () { }"

  test "missing effect is filled in with general" $
    desugarAndPrettyPrint "fn f() -> i32 { 1 }"
      `shouldBe` Just "general fn f() -> i32 { 1 }"

  test "default attribute argument is added if argument is missing" $
    desugarAndPrettyPrint "#[qasm_gate]\ngeneral fn myFun() -> () {}"
      `shouldBe` Just "#[qasm_gate(\"myFun\")]\ngeneral fn myFun() -> () { }"

  test "nested expressions round-trip through the canonical printer" $
    desugarAndPrettyPrint "fn add(x: i32) -> i32 { x + 1 }"
      `shouldBe` Just "general fn add(x: i32) -> i32 { (x + 1) }"

  test "const declarations desugar unchanged" $
    desugarAndPrettyPrint "const N: i64 = 4;"
      `shouldBe` Just "const N: i64 = 4;"
