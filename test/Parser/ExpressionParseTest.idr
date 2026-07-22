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

  test "basis string literal expressions" $
    parseAndPrettyPrint
      "fn states() {bs\"10110010\"; bs\"++----++\"; bs\"10+-+-001\"; bs\"iiiiIIIII\"}" `shouldBe`
      Just
        "fn states() { bs\"10110010\"; bs\"++----++\"; bs\"10+-+-001\"; bs\"iiiiIIIII\" }"

  test "basis string literals in inferred let bindings" $
    parseAndPrettyPrint
      "fn states() {let a = bs\"10110010\"; let b = bs\"++----++\"; let c = bs\"10+-+-001\"; let d = bs\"iiiiIIIII\";}" `shouldBe`
      Just
        "fn states() { let a = bs\"10110010\"; let b = bs\"++----++\"; let c = bs\"10+-+-001\"; let d = bs\"iiiiIIIII\"; }"

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

  test "block expression" $
    parseAndPrettyPrint "fn block() {{1}}" `shouldBe`
      Just "fn block() { { 1 } }"

  test "classical if expressions" $
    parseAndPrettyPrint "fn choose() {if ready {1} else if retry {2} else {3}}" `shouldBe`
      Just "fn choose() { if ready { 1 } else if retry { 2 } else { 3 } }"

  test "quantum if expressions are not yet supported" $
    parseErrorDetails "fn f() {qif q {1} qelse {0}}" `shouldBe`
      Just ("Quantum if expressions are not yet supported.",
            "test-fixture.rs", (1, 9), (1, 12))

  test "match expressions are not yet supported" $
    parseErrorDetails "fn f() {match value {0 => false}}" `shouldBe`
      Just ("Match expressions are not yet supported.",
            "test-fixture.rs", (1, 9), (1, 14))

  test "quantum match expressions are not yet supported" $
    parseErrorDetails "fn f() {qmatch q {0 => false}}" `shouldBe`
      Just ("Quantum match expressions are not yet supported.",
            "test-fixture.rs", (1, 9), (1, 15))

  test "loop while and for expressions" $
    parseAndPrettyPrint "fn control() {loop {break}; while ready {continue}; for x in values {x}}" `shouldBe`
      Just "fn control() { loop { break }; while ready { continue }; for x in values { x } }"

  test "semicolon-free non-final block-like expression statements" $
    parseAndPrettyPrint
      "fn control() {if ready {work();} loop {break;} while ready {continue;} for x in values {use_value(x);} finish()}" `shouldBe`
      Just
        "fn control() { if ready { work(); } loop { break; } while ready { continue; } for x in values { use_value(x); } finish() }"

  test "break continue and return expressions" $
    parseAndPrettyPrint "fn exits() {break 1; continue; return; return value}" `shouldBe`
      Just "fn exits() { break 1; continue; return; return value }"

  test "state if expressions are not yet supported" $
    parseErrorDetails "fn f() {sif state {1} else {0}}" `shouldBe`
      Just ("State if expressions are not yet supported.",
            "test-fixture.rs", (1, 9), (1, 12))

  test "state match expressions are not yet supported" $
    parseErrorDetails "fn f() {smatch state {0 => 1}}" `shouldBe`
      Just ("State match expressions are not yet supported.",
            "test-fixture.rs", (1, 9), (1, 15))

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

  test "struct literal expressions are not yet supported" $
    parseErrorDetails "fn f() {Point {x: 1, y: 2}}" `shouldBe`
      Just ("Struct literal expressions are not yet supported.",
            "test-fixture.rs", (1, 9), (1, 14))

  test "self expressions are not yet supported" $
    parseErrorDetails "fn f() {self.field}" `shouldBe`
      Just ("Self expressions are not yet supported.",
            "test-fixture.rs", (1, 9), (1, 13))

  test "controlled callable expressions are not yet supported" $
    parseErrorDetails
      "fn f() {ctrl(&q0, &q1).on(bs\"10\").apply(H)(&q2)}" `shouldBe`
      Just ("Control expressions are not yet supported.",
            "test-fixture.rs", (1, 9), (1, 13))

  test "controlled block expressions are not yet supported" $
    parseErrorDetails "fn f() {ctrl(&q0, &q1) {H(&q2);}}" `shouldBe`
      Just ("Control expressions are not yet supported.",
            "test-fixture.rs", (1, 9), (1, 13))

  test "adjoint callable expressions are not yet supported" $
    parseErrorDetails "fn f() {adjoint(f)(&q1, &q2, &q3)}" `shouldBe`
      Just ("Adjoint expressions are not yet supported.",
            "test-fixture.rs", (1, 9), (1, 16))

  test "adjoint block expressions are not yet supported" $
    parseErrorDetails "fn f() {adjoint {f();}}" `shouldBe`
      Just ("Adjoint expressions are not yet supported.",
            "test-fixture.rs", (1, 9), (1, 16))

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

  test "state literals are not yet supported" $
    parseErrorDetails "fn f() {zero}" `shouldBe`
      Just ("State literals are not yet supported.", "test-fixture.rs", (1, 9), (1, 13))
