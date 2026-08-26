module PostParseValidator.PostParseValidationTest

import Test.Simple

import Frontend.ASTPhases
import Frontend.PostParseValidation
import Frontend.Syntax.AST
import Parser.Helper

%default total

validationMessages : String -> Maybe (List String)
validationMessages source =
  case lexThenParse "test-fixture.rs" source of
    Nothing => Nothing
    Just sourceFile => Just (map interpolate (validateSourceFile sourceFile))

export
runPostParseValidationTests : IO ()
runPostParseValidationTests = runTests $ Test.do

  test "unknown attributes are rejected after parsing" $
    validationMessages "#[unknown]\nfn f() {}" `shouldBe`
      Just
        [ "test-fixture.rs:1:1: unknown attribute `unknown` " ++
          "(supported: qasm_gate, qasm_def)"
        ]

  test "mutable qubit references are rejected after parsing" $
    validationMessages "fn f(q: &mut qubit) {}" `shouldBe`
      Just
        [ "test-fixture.rs:1:9: `mut` is never written on a qubit reference; " ++
          "qubit references are mutable by default"
        ]

  test "break outside a loop is rejected after parsing" $
    validationMessages "fn f() {break;}" `shouldBe`
      Just ["test-fixture.rs:1:9: `break` outside of a loop"]

  test "continue outside a loop is rejected after parsing" $
    validationMessages "fn f() {continue;}" `shouldBe`
      Just ["test-fixture.rs:1:9: `continue` outside of a loop"]

  test "return in a constant initializer is rejected after parsing" $
    validationMessages "const N: i64 = return 4;" `shouldBe`
      Just ["test-fixture.rs:1:16: `return` outside of a function body"]

  test "a repeated attribute name is rejected after parsing" $
    validationMessages "#[qasm_gate]\n#[qasm_gate]\nfn f() {}" `shouldBe`
      Just
        [ "test-fixture.rs:2:1: attribute `qasm_gate` is already applied " ++
          "to this item"
        ]

  test "two distinct known attributes together are rejected after parsing" $
    validationMessages "#[qasm_gate]\n#[qasm_def]\nfn f() {}" `shouldBe`
      Just
        [ "test-fixture.rs:2:1: attribute `qasm_def` conflicts with " ++
          "another attribute already applied to this item (qasm_gate and " ++
          "qasm_def are mutually exclusive)"
        ]

  test "a repeated parameter name is rejected after parsing" $
    validationMessages "fn f(x: i32, x: i32) {}" `shouldBe`
      Just
        [ "test-fixture.rs:1:14: parameter `x` is already used earlier " ++
          "in this parameter list"
        ]

  test "valid contextual forms pass post-parse validation" $
    validationMessages
      "#[qasm_gate]\nfn f(x: &mut i32) {loop {break;} while ready {continue;} return}" `shouldBe`
      Just []
