module Parser.ExpressionParseTest

import Test.Simple

import Parser.Helper

%default total

export
runExpressionParseTests : IO ()
runExpressionParseTests = runTests $ Test.do

  test "function with a simple assignment statement" $
    parseAndPrettyPrint "fn assign() {x = 1;}" `shouldBe` Just "fn assign() { x = 1; }"

  test "function with compound assignment statements" $
    parseAndPrettyPrint "fn assign() {x += 1; x <<= 2; x &= 3;}" `shouldBe`
      Just "fn assign() { x += 1; x <<= 2; x &= 3; }"

  test "boolean literal expressions" $
    parseAndPrettyPrint "fn booleans() {true; false}" `shouldBe`
      Just "fn booleans() { true; false }"

  test "float literal expressions" $
    parseAndPrettyPrint "fn floats() {1.0; 12E+99_f64}" `shouldBe`
      Just "fn floats() { 1.0; 12E+99_f64 }"

  test "unit literal expression" $
    parseAndPrettyPrint "fn unit() {()}" `shouldBe` Just "fn unit() { () }"

  test "parenthesized expression" $
    parseAndPrettyPrint "fn parenthesized() {(value)}" `shouldBe`
      Just "fn parenthesized() { (value) }"

  test "tuple expressions" $
    parseAndPrettyPrint "fn tuples() {(value,); (1, true); (1, (2, 3),)}" `shouldBe`
      Just "fn tuples() { (value,); (1, true); (1, (2, 3)) }"

  test "array expressions" $
    parseAndPrettyPrint "fn arrays() {[]; [1, 2, 3]; [1, 2, 3,]}" `shouldBe`
      Just "fn arrays() { []; [1, 2, 3]; [1, 2, 3] }"

  test "repeated array expression" $
    parseAndPrettyPrint "fn repeated() {[0; 3]}" `shouldBe`
      Just "fn repeated() { [0; 3] }"

  test "unary operator expressions" $
    parseAndPrettyPrint "fn unary() {-x; !x; &x; &mut x}" `shouldBe`
      Just "fn unary() { (-x); (!x); (&x); (&mut x) }"

  test "nested unary and postfix expressions" $
    parseAndPrettyPrint "fn unary() {!!flag; -f(x); &values[i]}" `shouldBe`
      Just "fn unary() { (!(!flag)); (-f(x)); (&values[i]) }"

  test "arithmetic binary precedence" $
    parseAndPrettyPrint "fn arithmetic() {1 + 2 * 3; 8 / 2 % 3 - 1}" `shouldBe`
      Just "fn arithmetic() { (1 + (2 * 3)); (((8 / 2) % 3) - 1) }"

  test "shift comparison and equality precedence" $
    parseAndPrettyPrint "fn comparisons() {a + b << c < d == e}" `shouldBe`
      Just "fn comparisons() { ((((a + b) << c) < d) == e) }"

  test "bitwise and logical precedence" $
    parseAndPrettyPrint "fn logic() {a & b ^ c | d && e || f}" `shouldBe`
      Just "fn logic() { (((((a & b) ^ c) | d) && e) || f) }"

  test "all comparison operators" $
    parseAndPrettyPrint "fn comparisons() {a <= b; a > b; a >= b; a != b}" `shouldBe`
      Just "fn comparisons() { (a <= b); (a > b); (a >= b); (a != b) }"

  test "cast expressions" $
    parseAndPrettyPrint "fn casts() {x as i32; value as i32 as i64}" `shouldBe`
      Just "fn casts() { (x as i32); ((value as i32) as i64) }"

  test "cast precedence" $
    parseAndPrettyPrint "fn casts() {-x as i32 + y as i32}" `shouldBe`
      Just "fn casts() { (((-x) as i32) + (y as i32)) }"

  test "range expressions" $
    parseAndPrettyPrint "fn ranges() {1..5; 1..; ..5; ..=5; ..}" `shouldBe`
      Just "fn ranges() { (1..5); (1..); (..5); (..=5); (..) }"

  test "range endpoint precedence" $
    parseAndPrettyPrint "fn ranges() {1 + 2..3 * 4; ..x as i32}" `shouldBe`
      Just "fn ranges() { ((1 + 2)..(3 * 4)); (..(x as i32)) }"

  test "string literal expression" $
    parseAndPrettyPrint "fn string() {\"hello\"}" `shouldBe`
      Just "fn string() { \"hello\" }"

  test "name expressions" $
    parseAndPrettyPrint "fn names() {value; result}" `shouldBe`
      Just "fn names() { value; result }"

  test "builtin expressions" $
    parseAndPrettyPrint "fn builtins() {qalloc(); measr(q)}" `shouldBe`
      Just "fn builtins() { qalloc(); measr(q) }"

  test "function call expressions" $
    parseAndPrettyPrint "fn calls() {f(); f(x, y)}" `shouldBe`
      Just "fn calls() { f(); f(x, y) }"

  test "chained adjoint call expression" $
    parseAndPrettyPrint "fn chained() {adjoint(f)(q)}" `shouldBe`
      Just "fn chained() { adjoint(f)(q) }"

  test "postfix index field tuple-index and method-call expressions" $
    parseAndPrettyPrint "fn postfix() {a[i]; p.x; t.0; a.len()}" `shouldBe`
      Just "fn postfix() { a[i]; p.x; t.0; a.len() }"

  test "chained postfix expressions" $
    parseAndPrettyPrint "fn postfix() {values()[i].field.len()}" `shouldBe`
      Just "fn postfix() { values()[i].field.len() }"

  test "index and field assignment targets" $
    parseAndPrettyPrint "fn assign() {a[i] = 1; p.x = 2;}" `shouldBe`
      Just "fn assign() { a[i] = 1; p.x = 2; }"

  test "path expressions are not yet supported" $
    parseErrorDetails "fn f() {Data::Left}" `shouldBe`
      Just ("Path expressions are not yet supported.", "test-fixture.rs", (1, 9), (1, 13))

  test "byte literals are not yet supported" $
    parseErrorDetails "fn f() {b'a'}" `shouldBe`
      Just ("Byte literals are not yet supported.", "test-fixture.rs", (1, 9), (1, 13))

  test "byte string literals are not yet supported" $
    parseErrorDetails "fn f() {b\"hi\"}" `shouldBe`
      Just ("Byte string literals are not yet supported.", "test-fixture.rs", (1, 9), (1, 14))

  test "basis string literals are not yet supported" $
    parseErrorDetails "fn f() {bs\"0\"}" `shouldBe`
      Just ("Basis string literals are not yet supported.", "test-fixture.rs", (1, 9), (1, 14))

  test "state literals are not yet supported" $
    parseErrorDetails "fn f() {zero}" `shouldBe`
      Just ("State literals are not yet supported.", "test-fixture.rs", (1, 9), (1, 13))
