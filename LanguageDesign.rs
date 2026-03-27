
// (param?): bind value, get name let p:Param = param("theta1")

// (2) Reserved Keywords: 
  adjoint, affin, as, barrier, break, classical, ctrl, continue, discard, else, false, fn, for, general, if, import, in, let, lin, loop, measr, negctrl, pow, qalloc, qif, qelse, qmatch, reset, return, scratch, true, unitary, uncompute, while

// (3) Operators Reserved Symbols and Keywords: 
  '?', '&', '(', ')', ',', ';', '=', '+', '-', '*', '/', '%', '+=', '-=', '*=', '/=', '%=', '>', '>=', '<', '<=', '..', '()', ':', '[', ']', '{', '}', '==', '!=', '&&', '||', '!', '.', '..=', '::', '|', '^', '->', ':='

// (4) Quantum Gates Reserved Keywords: 
  Id, X, Y, Z, H, S, SDG, T, TDG, SX, SXDG, RX, RY, RZ, U1, U2, U3, CNOT, CY, CZ, CS, CSDG, CT, CTDG, CSX, CSXDG, CRX, CRY, CRZ, CU1, CU2, CU3, SWAP, RXX, RYY, RZZ, CCX, CSWAP, GPI, GPI2, MS


// (5) Apply simple quantum gates on some qubits:

H(q0);
CX(q0, q1);

// (6) Apply controlled gates:

ctrl(q0) H(q1);
negctrl(q0) H(q1);
ctrl(q0, q1) H(q2);

// (7) More complicated quantum gate patterns where resulting qubits are redeclared as fresh qubit variables:
// Also use use 'ctrl' and 'negtcrl' to apply controlled gates:
let (q0, q1, q2) = ctrl(q0) negctrl(q1) H(q2);
// Alternative syntax for declaring controlled gates using booleans arguments instead of 'ctrl' and 'negtcrl':
let (q0, q1, q2) = ctrl(q0 = true, q1 = false) H(q2);
// Control can apply to a block of gates:
ctrl(q0, q1) {
  H(q2);
  CX(q2, q3);
}

// (8) Comments syntax for the new language:

  // single line commnent

  /*
     multi-line comment
  */

// (9) Variable declaration and assigment:
// The variable type can be specified or inferred

  let my_var = 5;

  let x:i32 = 3;
  x = 2 + 5;

// (10) Types variable delaration:

  let my_var : i32 = 5;

// (11) Operators:

  let add = 5 + 3;
  let sub = 10 - 4;
  let mul = 6 * 2;
  let div = 12 / 3;
  let rem = 10 % 3;

// (12) More operators:

  x += 5;
  x -= 2;
  x *= 2;
  x /= 3;
  x %= 4;

// (13) Boolean variable:

  let my_var: bool = true;

// (14) If/Else syntax:

  if 7 > 5 {
    // do something
  } else {
    // do something else
  }

// (15) Syntax for declaring loops:

  loop {
      if count == 3 {
          break;
      }
      count += 1;
  }

// (16) Syntax for declaring loops that return a value:

  let result = loop {
    println!("Hello!");

    if count == 3 {
      break count;
    } else {
      continue;
    }

    count += 1;
  };

// (17) While loop syntax:

  while count <= 5 {
    count += 1;
  }
  
// (18) For loop syntax:

  for i in 1..6 {
    // do something
  }

// (19) Functions syntax:

  fn function_name() {
    // code to be executed
  }
  
//  Function returning some variable:
  
  fn f(x: i32) -> i32 {
  	x + 1
   }

  // Silq qfree -> classical
  // Does not allocate qubits
  // Does not apply quantum gates
  // Does not measure
  // Does not touch quantum state at all
  // opt-in promise (guardrail), like "const" in C++
  classical fn f(x: i32) -> i32 {
    x + 1
  }


  // Silq mfree -> unitary
  // guarantees function does not perform measurement and does not depend on measured values
  // opt-in promise (guardrail), like "const" in C++
  unitary fn f(q: qubit) -> qubit {
    let q = X(q);
    let q = H(q);
    q
  }

  // nice in public libraries: “yes, this measures”
  // opt-in promise (guardrail), like "const" in C++
  general fn sample(q: qubit) -> bit {
    measure(q)
  }

  
// // At IR level:
//   data Effect
//     = Classical
//     | Unitary
//     | MayMeasure

//   join : Effect -> Effect -> Effect
//   join Classical   e            = e
//   join e           Classical    = e
//   join Unitary     Unitary      = Unitary
//   join Unitary     MayMeasure   = MayMeasure
//   join MayMeasure  Unitary      = MayMeasure
//   join MayMeasure  MayMeasure   = MayMeasure

//   Prog : Effect -> Type

//   compose : Prog e1 -> Prog e2 -> Prog (join e1 e2)
//   // Subtyping in an effect lattice: Classical < Unitary < MayMeasure
//   //You could even prove: Classical implies Unitary


// (20) Variable declared in the scope of a function:

  fn myFunction() {
    let message = "Hello!";
  }
  
// (21) Function syntax with typed arguments:

  fn f(x: (i32, bool)) {
    let (n, flag) = x;
  }  
  
// (22) Function returning a value:
fn f() {
    let x = 2.0;
    x
}

// (23) Alternative syntax for function returning a value:
fn f() {
    let x = 2.0;
    return x
}
  
// (24) Declaring tuples:

  let t = (1, 3.14, true);
  let t: (i32, f64, bool) = (1, 3.14, true);
  
// (25) Tuples positional indexing:

  let x = t.0;
  let y = t.1;
  let z = t.2;

// (26) Extracting variables from tuples:
  
  let (a, b, c) = (1, 2, 3);
  let (x, _, z) = (1, 2, 3);
  let (q0, q1, q2) = (H(q0), H(q1), H(q2))
  
// (27) Unit type syntax:

  let u: () = ();
  
// (28) Declaring Arrays literal:

let a = [1, 2, 3];
let b: [i32; 4] = [10, 20, 30, 40];

// (29) Arrays declared using a repeat syntax:

let zeros = [0; 8];         // [0, 0, 0, 0, 0, 0, 0, 0]
let flags = [true; 3];      // [true, true, true]

// (30) Array access and length:

let a = [1, 2, 3];
let first = a[0];
let n = a.len();

// (31) Other quantum specific constructs:

let q: qubit = qalloc();
let qs: [qubit; 8] = qalloc(8); 

let b: bit = measr(q);
let bs: [bit; 8] = measr(qs);

// Bitstrings: b"10110010" (See OpenQasm3 for reference)
b"0101"

// (32) Bit can appear in conditions:

if b { X(q); }        

if b == 1 { X(q); }     


// (33) Ranges:
    a..b (exclusive)
    a..=b (inclusive)

// (34) Bitwise operations:

let and_ = a & b;
let or_  = a | b;
let xor_ = a ^ b;

// (35) Blocks as expressions:

  let x = if flag { 1 } else { 2 };
  
// (36) Pattern matching:

fn main() {
  let day = 4;

  match day {
    1 => { /* todo */ },
    2 => { /* todo */ },
    3 => { /* todo */ },
    4 => { /* todo */ },
    5 => { /* todo */ },
    6 => { /* todo */ },
    7 => { /* todo */ },
    _ => { /* todo */ },
  }
}

match b {
  true => { /* todo */ }
  false => { /* todo */ }
}

// (37) Type casting:
 
 let b:Bit = 1;
 let x = b as i32;

// (38) Qubit allocation:

qalloc() -> Qubit
qalloc(n: u32) -> qubit[n]

// (39) Reset:

reset(q: Qubit) -> Qubit
reset(qs: [qubit; n]) -> [qubit; n]

// (40) Measure:

measr(q: qubit) -> (bit, qubit)
measr(qs: [qubit; n]) -> ([bit; n], [qubit; n])

// (41) An example of quantum flow:

fn bell_pair() -> (bit, bit) {
  let q0: qubit = qalloc();
  let q1: qubit = qalloc();

  H(q0);
  CX(q0, q1);

  let (b0, q0) = measr(q0);
  let (b1, q1) = measr(q1);

  // q0, q1 still exist (collapsed); can reset/discard later if you want
  (b0, b1)
}

// (42) Allocating quantum register, both notations should work: 

// dynamic length
let qs: [qubit] = qalloc(8);

// static length
let qs: [qubit; 8] = qalloc(8);

// static length alias
let qs: [qubit; 8] = qalloc(8);

// (43) import statements
import "my_library.lf";

// (44) barrier statement, works like OpenQasm3 barrier
barrier(q0, q1, q2);

// (45) discard statement, discards qubits without resetting them
discard(q0);

discard(qs);

// (46) uncompute statement, uncomputes qubits without resetting them
uncompute(q0);

// (47) adjoint statement, applies the inverse of a block of gates

adjoint {
  H(q0);
  CX(q0, q1);
}

let (q0, q1, q2) = adjoint (H(q0), H(q1), H(q2))

let q = adjoint H(q0)

let (q0, q1, q2) = adjoint f(q0, q1, q2);

// (48) scratch qubit, gets uncomputed when goes out of scope. 
// The qubit type can be specified or inferred
let scratch q: qubit = qalloc();

let scratch q = qalloc();

let scratch qs: [qubit; 8] = qalloc(8);

let scratch qs = qalloc(8);

// (49) function annottated to contain only cassical code, no quantum operations allowed, including no qubit allocation, no quantum gates, no measurement, no dependence on measured values.

  classical fn f(x: i32) -> i32 {
  	x + 1
  }

// (50) function annotated to contain only unitary code, no measurement allowed, including no dependence on measured values.

  unitary fn f(q: qubit) -> qubit {
    let q = X(q);
    let q = H(q);
    q
  }

// (51) function annotated to contain general code, can contain measurements and depend on measured values.

  general fn sample(q: qubit) -> bit {
    let q = H(q);
    measure(q)
  }

// (52) Declaring a qubits as linear, meaning they must be used exactly once, no copying or discarding allowed. Default is linear, so the 'lin' keyword is optional.  

  let lin q: qubit = qalloc();

  let lin qs = qalloc(8);

// (53) Declaring qubits as affine, meaning they can be used at most once, no copying allowed, but discarding is allowed.

  let affin q: qubit = qalloc();

  let affin qs = qalloc(8);

// (54) Declaring bit strings

let bs = b"10110010";
let bs: [bit; 8] = b"10110010";
let bs: [bit; 8] = [0, 1, 1, 0, 0, 1, 0, 0];

// (55) quantum conditional statements:

  qif (c) {
    H(t);
  } qelse {
    X(t);
  }

  // (56) quantum match statements:

  qmatch ctrl {
    0 => {
        U0(data);
    }
    1 => {
        U1(data);
    }
  }

  qmatch ctrl {
    b"00" => { U00(data); }
    b"01" => { U01(data); }
    b"10" => { U10(data); }
    b"11" => { U11(data); }
  }

  qmatch ctrl {
    0 => { U00(data); }
    1 => { U01(data); }
    2 => { U10(data); }
    3 => { U11(data); }
  }