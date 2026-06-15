//////////////////////////////////////
// Leaf Language Syntax Specification
//////////////////////////////////////

// Leaf is deliberately designed to replicate Rust’s basic syntax, with minimal extensions for quantum programming which are meant to look and feel like Rust.

////////////////////////////////////////////////////////////////////////////////
// (1) Comments syntax for the Leaf language follows the same syntax as Rust:
////////////////////////////////////////////////////////////////////////////////

  // single line comment

  /*
     multi-line comment
  */

///////////////////////////////////
// (2) Basic Leaf Language Syntax
///////////////////////////////////

// Leaf follows Rust-style semicolon rules:
// most statements end with ';', while the final expression of a block or function body
// may omit ';' when its value is returned by that block or function.

// Parentheses, square brackets and curly braces follow the same rules from Rust.

//////////////////////////
// (3) Reserved Keywords: 
//////////////////////////

adjoint, affine, as, barrier, basis, break, classical, clean, ctrl, coisometry, const, continue, discard, else, enum, ensures, false, fn, for, general, if, impl, in, isolated, isometry, let, linear, loop, match, minusi, measr, mod, mut, minus, one, plus, plusi, pub, product, qalloc, qif, qelse, qenum, qmatch, requires, reset, return, scratch, sif, selse, self, separable, smatch, stabilized, struct, supports, then, true, unitary, uncompute, uncompsafe, use, weaken, while, zero, _

//////////////////////////////////////////////////////////////
// (4) Reserved delimiters, punctuation, and operator tokens:
//////////////////////////////////////////////////////////////

'(', ')', '[', ']', '{', '}', ',', ';', ':', '::', '.', '->', '=', ':=', '+=', '-=', '*=', '/=', '%=', '+', '-', '*', '/', '%', '==', '!=', '>', '>=', '<', '<=', '=>', '>>', '>>=',  '<<', '<<=', '!', '&&', '||', '&', '|', '^', '..', '..=', '&=', '|=', '^=', '#'

///////////////////////////////////////
// (5) Built-in primitive type syntax:
///////////////////////////////////////

// quantum computing specific:
bit, qubit

// additional quantum types:
// - angle type is similar to OpenQasm3's angle type
// - symbolic compile-time parameters param type is similar to Qiskit's Parameter type
angle32, angle64, param

// signed integer types:  
i8, i16, i32, i64, i128

// unsigned integer types:
u8, u16, u32, u64, u128

// floating-point types:
f32, f64

// boolean type:
bool

// unit type:
()

////////////////////////////////
// (6) Syntax for Basic Types
////////////////////////////////

let f : f32 = 1.234567;
let d : f64 = -1.2345678901234567;

// inferred types for floating point literals
let d = -1000.0;

let i : i8 = -1;
let i : i16 = -1;
let i : i32 = 1;
let i : i64 = 1;
let i : i128 = -1;

// inferred types for integer literals
let i = -7;

// qubit declaration 
let q: qubit = qalloc();

// inferred type for qubit allocation
let q = qalloc();

let u : u8 = 1;
let u : u16 = 1;
let u : u32 = 1;
let u : u64 = 1;
let u : u128 = 1;

let unit : () = ();

// inferred type for unit literal
let unit = ();

// syntax for declaring parameters, here "param" is a builtin function:
let theta : param = Param("theta");

// Leaf does not yet support strings but strings are required lexically (see below string usages: "qasm_subroutine_name", "X", "Z(q0) * Z(q1)")
// also see this syntax is accepted for declaring parameters with dynamic names using string interpolation:
let i: i32 = 1;
let theta: param = Param(format!("theta_{i}"));

// var assignment syntax for basic types:
let mut x : i32 = 0;
x = 5;

// var assignment syntax for arrays and tuples:
let mut x : [i32; 2] = [0, 0];
x[0] = 10;

// assignment for tuple members:
let mut t : (i32, f64) = (0, 0.0);
t.0 = 3;

// shared reference
let x = 10;
let r: &i32 = &x;

// mutable reference
let mut x = 10;
let r: &mut i32 = &mut x;

// const variables
const PI: f64 = 3.141592653589793;
const FIVE: i32 = 5;

///////////////////////////////////
// (7) Syntax for declaring arrays
///////////////////////////////////

// canonical array declarations
let bs: [bool; 4] = [true, false, true, false];
let is: [i32; 3] = [1, 2, 3];
let us: [u64; 2] = [10, 20];
let fs: [f64; 3] = [1.0, 2.0, 3.0];
let bits: [bit; 3] = [1, 0, 1];
let qubits: [qubit; 2] = qalloc(2);
let linear qubits: [qubit; 2] = qalloc(2);
let affine qubits: [qubit; 2] = qalloc(2);
let angles: [angle64; 2] = [3.14, 1.57];
let units: [(); 5] = [(), (), (), (), ()];
let params: [param; 2] = [Param("theta"), Param("phi")];

// array declarations with inferred type
let bools = [true, false, true, false];
let ints  = [1, 2, 3];
let zeros = [0; 3];
let fs = [1.0, 2.0, 3.0];
let qubits = qalloc(2);
let units = [(), (), (), (), ()];

////////////////////////////////////////
// (8) Array member access and length:
////////////////////////////////////////

let a = [1, 2, 3];
let first = a[0];
let n = a.len();

/////////////////////////////////////////////////////////////////
// (9) Syntax for allocating qubits and working with bits/qubits
/////////////////////////////////////////////////////////////////

// explicit type annotations for qubits and bits
let q: qubit = qalloc();
let qs: [qubit; 1] = qalloc(1);
let qs: [qubit; 3] = qalloc(3);
let b : bit = measr(q);
let b : bit = 0;
let bs : [bit; 3] = measr(qs);

// inferred types for qubits and bits
let q = qalloc();
let qs = qalloc(3);
let b = measr(q);

//////////////////////////////////////////////////////////////////////////
// (10) Declaring basis strings literals (arrays of 0 and 1, +, -, I, i):
//////////////////////////////////////////////////////////////////////////

let state = bs"10110010";
let state = bs"++----++";
let state = bs"10+-+-001";
let state = bs"iiiiIIIII";

//////////////////////////////////////////////
// (11) Syntax for type qualifiers for qubits
//////////////////////////////////////////////

// linear qubits: must be consumed exactly once, no copying or implicit discarding allowed (discarding must be explicit via the discard keyword)
// this is the default, so the 'linear' keyword is optional
let linear q: qubit = qalloc();
let linear qs: [qubit; 2] = qalloc(2);

// affine qubits: must be consumed at most once, no copying allowed, but implicit discarding is allowed
let affine q: qubit = qalloc();
let affine qs: [qubit; 2] = qalloc(2);

// scratch qubits are automatically uncomputed when they go out of scope and reclaimed automatically (no explicit discard is needed)
let scratch q: qubit = qalloc();
let scratch qs: [qubit; 2] = qalloc(2);

// linear/affine type qualifiers can be combined with scratch
// both "scratch linear" and "linear scratch" are accepted syntax for linear scratch qubits, and the same applies for affine scratch qubits
let scratch linear q: qubit = qalloc();
let scratch linear qs: [qubit; 2] = qalloc(2);
let linear scratch qs: [qubit; 2] = qalloc(2);
let scratch affine q: qubit = qalloc();
let scratch affine qs: [qubit; 2] = qalloc(2);
let linear scratch q: qubit = qalloc();
let affine scratch q: qubit = qalloc();
let affine scratch qs: [qubit; 2] = qalloc(2);

/////////////////////////////////////////
// (12) Syntax for working with Quantum Gates
/////////////////////////////////////////

// canonical gate application syntax:
let q: qubit = H(q);
// gate application syntax with inferred types:
let q = H(q);
// borrowing qubit syntax if the qubit will be needed later.
// Note that Leaf borrows for qubits intentionally differ from Rust borrows since qubits are mutable by default
H(&q);

// parameterized gates:
let q: qubit = U3(1.0, 2.0, 3.0, q);
let q = U3(1.0, 2.0, 3.0, q);
U3(1.0, 2.0, 3.0, &q);

// two-qubit gates:
let (q0, q1) : (qubit, qubit) = CX(q0, q1);
let (q0, q1) = CX(q0, q1);
CX(&q0, &q1);

///////////////////////////////////////////
// (13) Built-in quantum gate identifiers:
///////////////////////////////////////////

Id, X, Y, Z, H, S, SDG, T, TDG, SX, SXDG, RX, RY, RZ, U1, U2, U3, CNOT, CX, CY, CZ, CS, CSDG, CT, CTDG, CSX, CSXDG, CRX, CRY, CRZ, CU1, CU2, CU3, SWAP, RXX, RYY, RZZ, CCX, CSWAP, GPI, GPI2, MS, ZZ

// Single-Qubit Gates
let q: qubit = Id(q);
let q: qubit = X(q);
let q: qubit = Y(q);
let q: qubit = Z(q);
let q: qubit = H(q);
let q: qubit = S(q);
let q: qubit = SDG(q);
let q: qubit = T(q);
let q: qubit = TDG(q);
let q: qubit = SX(q);
let q: qubit = SXDG(q);

// Parametric Single-Qubit Gates
// the angle input arguments can be of type: param, angle32, angle64 or floating point numbers
let q: qubit = RX(1, q);
let q: qubit = RY(1.0, q);
let q: qubit = RZ(1.0, q);
let q: qubit = U1(1.0, q);
let q: qubit = U2(1.0, 2.0, q);
let q: qubit = U3(1.0, 2.0, 3.0, q);

// Controlled Gates
// the angle input arguments can be of type: param, angle32, angle64 or floating point numbers
let (q0, q1): (qubit, qubit) = CNOT(q0, q1);
let (q0, q1) = CX(q0, q1);
let (q0, q1) = CY(q0, q1);
let (q0, q1) = CZ(q0, q1);
let (q0, q1) = CS(q0, q1);
let (q0, q1) = CSDG(q0, q1);
let (q0, q1) = CT(q0, q1);
let (q0, q1) = CTDG(q0, q1);
let (q0, q1) = CSX(q0, q1);
let (q0, q1) = CSXDG(q0, q1);

// Controlled Gates with parameters
// the angle input arguments can be of type: param, angle32, angle64 or floating point numbers
let (q0, q1) = CRX(1.0, q0, q1);
let (q0, q1) = CRY(1.0, q0, q1);
let (q0, q1) = CRZ(1.0, q0, q1);
let (q0, q1) = CU1(1.0, q0, q1);
let (q0, q1) = CU2(1.0, 2.0, q0, q1);
let (q0, q1) = CU3(1.0, 2.0, 3.0, q0, q1);

// Two-Qubit Interaction Gates
// the angle input arguments when present can be of type angle32, angle64 or floating point numbers
let (q0, q1) = SWAP(q0, q1);
let (q0, q1) = RXX(1.0, q0, q1);
let (q0, q1) = RYY(1.0, q0, q1);
let (q0, q1) = RZZ(1.0, q0, q1);

// Three-Qubit Gates
let (q0, q1, q2) = CCX(q0, q1, q2);
let (q0, q1, q2) = CSWAP(q0, q1, q2);

// Ion-Native Gates
// the angle input arguments can be of type: param, angle32, angle64 or floating point numbers
let q: qubit = GPI(1.0, q);
let q: qubit = GPI2(1.0, q);
let (q0, q1) = MS(1.0, 2.0, q0, q1);
let (q0, q1) = ZZ(1.0, q0, q1);

////////////////////////////////////////////////////////
// (14) Apply higher-order control gate modifiers: ctrl
////////////////////////////////////////////////////////

// canonical controlled gate
let (q0, q1, q2): (qubit, qubit, qubit) = ctrl(q0, q1).apply(H)(q2);
// controlled gate with inferred types:
let (q0, q1, q2) = ctrl(q0, q1).apply(H)(q2);
// controlled gate with borrowed qubits:
ctrl(&q0, &q1).apply(H)(&q2);
// block syntax
ctrl(&q0, &q1) {
  H(&q2);
}

// applying controlls on "10" state
let (q0, q1, q2) = ctrl(q0, q1).on(bs"10").apply(H)(q2);
// same with borrowed qubits:
ctrl(&q0, &q1).on(bs"10").apply(H)(&q2);
// block syntax
ctrl(&q0, &q1).on(bs"10") {
  H(&q2);
}

// apply controls on some generic function of qubits:
let (q0, q1, q2, q3) = ctrl(q0, q1).apply(f)(q2, q3);
// same with borrowed qubits:
ctrl(&q0, &q1).apply(f)(&q2, &q3);
// block syntax
ctrl(&q0, &q1) {
  f(&q2, &q3);
}

// apply controls on some generic function of qubits on ++ state:
let (q0, q1, q2, q3) = ctrl(q0, q1).on(bs"++").apply(f)(q2, q3);
// same with borrowed qubits:
ctrl(&q0, &q1).on(bs"++").apply(f)(&q2, &q3);
// block syntax
ctrl(&q0, &q1).on(bs"++") {
  f(&q2, &q3);
}

//////////////////////////
// (15) Adjoint Operator:
//////////////////////////

// adjoint as higher order function:
let f_adjoint = adjoint(f);

// applying adjoint to generic function of qubits:
let (q1, q2, q3): (qubit, qubit, qubit)  = adjoint(f)(q1, q2, q3);
// adjoint with inferred types
let (q1, q2, q3)  = adjoint(f)(q1, q2, q3);
// adjoint with borrowed qubits:
adjoint(f)(&q1, &q2, &q3);
// block syntax for adjoint:
adjoint {
    f(&q1, &q2, &q3);
}
// applying adjoint to built-in gates:
adjoint {
    H(&q1);
    CT(&q1, &q2)
}
// or equivalently:
adjoint(CT)(&q1, &q2);
adjoint(H)(&q1);

//////////////////////////////
// (16) Arithmetic Operators:
//////////////////////////////

let add = 5 + 3;
let sub = 10 - 4;
let mul = 6 * 2;
let div = 12 / 3;
let rem = 10 % 3;

x += 5;
x -= 2;
x *= 2;
x /= 3;
x %= 4;

//////////////////////////////
// (17) Boolean Operators:
//////////////////////////////

let c = !a;
let d = a && b;
let e = a || b;


////////////////////////////
// (18) Bitwise operations:
////////////////////////////

let and_bit : bit = a & b;
let or_bit  : bit = a | b;
let xor_bit : bit = a ^ b;

x &= y;
x |= y;
x ^= y;

let x = a << 2;
let y = a >> 1;

x <<= 1;
x >>= 1;

//////////////////////////////////
// (19) Classical If/Else syntax:
//////////////////////////////////

  // Like in Rust,else associates with the nearest preceding unmatched if.
  // Like in Rust, else is optional
  // Like in Rust, if/else branches are either expressions or statements

// conditions must be boolean expressions or bits
if 7 > 5 {
  // do something
} else { // else branch is optional
  // do something else
}

//a bit may be used as a classical condition, where 0 means false and 1 means true.
if b { fun(&q); }
// this is syntactic sugar for:
if b == 1 { fun(&q); }

// if else syntax:
let sign = if x < 0 {
  f1()
} else if x == 0 {
  f2()
} else {
  f3()
};

// if else expression syntax:
if x < 0 {
  -1
} else if x == 0 {
  0
} else {
  1
};

// Like in Rust, else associates with the nearest preceding unmatched if. 

///////////////////////////////////////////////
// (20) qif/qelse quantum conditionals syntax:
///////////////////////////////////////////////

  // Like in Rust, qelse associates with the nearest preceding unmatched qif.
  // Like in Rust, qelse is optional
  // Like in Rust, qif/qelse branches are either expressions or statements

  // q is a qubit and must be borrowed
  qif &q {
    // some unitary operation(s)
    X(&t);
  } qelse {
    // some other unitary operation(s)
    H(&t);
  }

  let (q1, q2, q3) = qif q1 expression1(q2, q3) qelse expression2(q2, q3);

///////////////////////////////////////////////
// (21) sif/selse quantum conditionals syntax:
///////////////////////////////////////////////

  // Like in Rust, selse associates with the nearest preceding unmatched sif.
  // Unlike in Rust, selse is NOT optional
  // Unlike in Rust, sif/selse branches are always expressions, no statement branches allowed.

  let qs = sif &q {
    // some quantum state expression
  } selse {
    // some other quantum state expression
  }

  let (q1, q2, q3) = sif q1 then expression1(q2, q3) selse expression2(q2, q3);

   // state expressions are built using zero/one/plus/minus/plusi/minusi basis string literals, phase() function for applying complex phases, and tensor operator for combining states of multiple qubits:
  sif q then
    (zero + one).tensor(zero - phase(pi/2) * one)
  selse
    (plus - minus).tensor(plus + phase(pi/2) * minus);

///////////////////////////////////////////
// (22) Classical Rust style match syntax:
///////////////////////////////////////////

match x {
    1 => foo(),   // comma required
    2 => bar(),   // comma required
    _ => baz(),   // trailing comma optional
}

match x {
    1 => { foo(); }   // comma optional
    2 => { bar(); }
    _ => { baz(); }
}

fn main() {
  let day = 4;

  match day {
    1 => { day_is_monday(); }
    2 => { day_is_tuesday(); }
    3 => { day_is_wednesday(); }
    4 => { day_is_thursday(); }
    5 => { day_is_friday(); }
    6 => { day_is_saturday(); }
    7 => { day_is_sunday(); }
    _ => { day_is_invalid(); }
  }
}

match boolflag {
  true => { /* todo */ }
  false => { /* todo */ }
}

match x {
  n if n > 0 => { /* positive */ }
  _ => { /* other */ }
}

////////////////////////////////////////////////
// (23) Quantum match style expressions qmatch:
////////////////////////////////////////////////

let (qs, q1, q2, q3) = qmatch qs {
  bs"00" => f00(q1, q2, q3),
  bs"01" => f01(q1, q2, q3),
  bs"10" => f10(q1, q2, q3),
  bs"11" => f11(q1, q2, q3),
}

qmatch &qs {
  bs"0+" => f00(&q1, &q2, &q3),
  bs"0-" => f01(&q1, &q2, &q3),
  bs"1+" => f10(&q1, &q2, &q3),
  bs"1-" => f11(&q1, &q2, &q3),
}

// following syntax integer based branch condition syntax is also supported:

qmatch &qs {
  0 => f00(&q1, &q2, &q3),
  1 => f01(&q1, &q2, &q3),
  2 => f10(&q1, &q2, &q3),
  3 => f11(&q1, &q2, &q3),
}

// where the above is the same as:

qmatch &qs {
  bs"00" => f00(&q1, &q2, &q3),
  bs"01" => f01(&q1, &q2, &q3),
  bs"10" => f10(&q1, &q2, &q3),
  bs"11" => f11(&q1, &q2, &q3),
}

// Like in Rust, wildcard patterns are supported for qmatch: 
_ => f()

////////////////////////////////////////////////
// (24) Quantum match style expressions smatch:
////////////////////////////////////////////////

// Unlike in Rust, wildcard patterns are NOT supported for smatch. 

smatch &q {
  0 => state_expression0(data),
  1 => state_expression1(data),
}

smatch &qs {
  bs"00" => state_expression_00(data),
  bs"01" => state_expression_01(data),
  bs"10" => state_expression_10(data),
  bs"11" => state_expression_11(data),
}

smatch &qs {
  0 => state_expression_0(data),
  1 => state_expression_1(data),
  2 => state_expression_2(data),
  3 => state_expression_3(data),
}

// state expressions are built using zero/one/plus/minus/plusi/minusi basis string literals, phase() function for applying complex phases, and tensor operator for combining states of multiple qubits.
smatch &qs {
  bs"00" => (zero + one).tensor(zero - phase(pi/2) * one),
  bs"01" => (plus - minus).tensor(plus + phase(pi/2) * minus),
  bs"10" => (zero - one).tensor(zero + phase(pi/2) * one),
  bs"11" => (plus - minus).tensor(plus - phase(pi/2) * minus),
}

////////////////////////////////
// (25) Rust block expressions:
////////////////////////////////

let x = {
  let a = 1;
  let b = 2;
  a + b
};

let unit = {
  let a = 1;
};

///////////////////////////////////
// (26) Syntax for declaring loops:
///////////////////////////////////

let mut count = 0;
loop {
    if count == 3 {
        break;
    }
    count += 1;
}

///////////////////////////////////////////////////////
// (27) Syntax for declaring loops that return a value:
///////////////////////////////////////////////////////

let mut count = 0;
let result = loop {

    if count == 3 {
        break count;
    }

    count += 1;
};

///////////////////////////
// (28) While loop syntax:
///////////////////////////

while count <= 5 {
  count += 1;
}

////////////////
// (29) Ranges:
///////////////

// exclusive range:
a..b

// inclusive range:
a..=b

// range from 1 onward
1..     

// range to: everything before 5
..5

// range to inclusive: everything up to and including 5
..=5

// full range
..      

// reverse range
(0..n).rev()

/////////////////////////
// (30) For loop syntax:
/////////////////////////

// using a range in a for loop:

for i in 1..6 {
  // do something
}

/////////////////////////
// (31) Declaring tuples:
/////////////////////////

let t: (i32, f64, bool) = (1, 3.14, true);
let t = (1, 3.14, true);
  
////////////////////////////////////
// (32) Tuples positional indexing:
////////////////////////////////////

let x = t.0;
let y = t.1;
let z = t.2;

// signature of CNOT is: fn CNOT(q0: qubit, q1: qubit) -> (qubit, qubit)
let qubits = CNOT(q0, q1);
let q0 = qubits.0;
let q1 = qubits.1;

//////////////////////////////////////////
// (33) Extracting variables from tuples:
//////////////////////////////////////////

let (a, b, c) = (1, 2, 3);
let (x, _, z) = (1, 2, 3);
let (q0, q1, q2) = (H(q0), H(q1), H(q2));

///////////////////////
// (34) If expressions
////////////////////////

let boolflag = true;
let x : i32 = if boolflag { 1 } else { 2 };

///////////////////////
// (35) Type casting:
///////////////////////

let b : bit = 1;
let x = b as i32;

//////////////////////
// (36) Reset qubits:
//////////////////////

let q: qubit = qalloc();
let q = reset(q);
reset(&q);

let qs: [qubit; 3] = qalloc(3);
// qubits are consumed and returned in reset state
let qs = reset(qs);
// qubits are borrowed and reset in place
reset(&qs);

//////////////////////
// (37) Discard qubits:
//////////////////////

let q: qubit = qalloc();
discard(q);

let qs: [qubit; 3] = qalloc(3);
discard(qs);

/////////////////////////////////////////////////////////////////////////////////
// (38) uncompute qubits: reverse the reversible computation that produced these
// qubits, returning them to |0⟩ when the compiler can verify that this is valid.
//////////////////////////////////////////////////////////////////////////////////

let q: qubit = qalloc();
// qubit is consumed and returned after valid uncomputation
let q = uncompute(q);
// qubit is borrowed and uncomputed in place
uncompute(&q);

let qs: [qubit; 3] = qalloc(3);
let qs = uncompute(qs);
uncompute(&qs);

//////////////////////////////////////////////////////////
// (39) Weakening qubits: demote linear qubits to affine:
//////////////////////////////////////////////////////////

let linear q: qubit = qalloc();
let affine q = weaken(q);

let linear qs: [qubit; 3] = qalloc(3);
let affine qs = weaken(qs);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// (40) ':=' marks the resulting qubit binding as automatically uncomputed when the enclosing function returns:
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let q: qubit := fun(q);
let qs: [qubit; 3] := fun(qs);

//////////////////////////
// (41) Measuring qubits:
//////////////////////////

let q: qubit = qalloc();
// qubit is consumed permanently
let b : bit = measr(q);
// qubit is borrowed
let b : bit = measr(&q);

let qs: [qubit; 3] = qalloc(3);
// qubits are consumed permanently
let bs : [bit; 3] = measr(qs);
// qubits are borrowed
let bs : [bit; 3] = measr(&qs);

// casting bits array measurement results to a tuple of bits:
let (q0, q1, q2) = measr(qs);

////////////////////////
// (42) Barrier syntax:
////////////////////////

barrier();
let (q0, q1, q2) = barrier(q0, q1, q2);
barrier(&q0, &q1);

////////////////////////////////////////////////////////////////////
// (43) declaring and using modules and imports follows Rust syntax:
////////////////////////////////////////////////////////////////////

mod my_module {
    pub fn helper() -> qubit {
        let q = qalloc(); // some code
        q
    }
}

mod my_library;

use my_library::helper;

fn main() {
    let q = helper();
    discard(q);
}

//////////////////////////
// (44) Functions syntax:
//////////////////////////

fn function_name() {
  // code to be executed
}

// a const fn is a function that may be evaluated at compile time:
const fn square(x: i32) -> i32 {
    x * x
}

///////////////////////////////////////
// (45) Function with typed arguments:
///////////////////////////////////////

fn add(x: i32, y: i32) -> i32 {
  x + y
}

//////////////////////////////////////////
// (46) Function returning some variable:
//////////////////////////////////////////

fn f(x: f32) -> f32 {
  let y = 2.0;
  x + y
}

///////////////////////////////////////////////////////////
// (47) Alternative syntax for function returning a value:
///////////////////////////////////////////////////////////
fn f() -> f64 {
    let x = 2.0;
    return x;
}

//////////////////////////////////////////////////////
// (48) Variable declared in the scope of a function:
//////////////////////////////////////////////////////
fn my_function() {
  let i = 10;
}

///////////////////////////////////////////////////////////
// (49) Functions using references and mutable references:
///////////////////////////////////////////////////////////

struct Person {
    id: i32,
    age: u32,
}

fn my_function(person: &Person) {
    // some code
}

fn my_function(person: &mut Person) {
    // some code
}

//////////////////////////////////////////////////////////
// (50) mutable variable declared in a local block scope:
//////////////////////////////////////////////////////////

{
  let mut x = 0;
  x = x + 1;
}

fn f(mut x: i32) -> i32 {
  x += 1;
  x
}

//////////////////////////////////////////////////////////////////////////////
// (51) Function Effect Qualifiers: classical, uncompsafe, unitary, general
//////////////////////////////////////////////////////////////////////////////

// function effects are optional Rust style function qualifiers used by the Leaf type checker to verify Leaf code.
// Function qualifiers appear before fn keyword and cannot be combined with each other

// no quantum operations allowed
classical fn f(x: i32) -> i32 {
  x + 1
}

// only uncomputation-safe quantum operations allowed
uncompsafe fn f(q: qubit) -> qubit {
  let q = X(q);
  q
}

// only unitary quantum operations allowed
unitary fn f(q: qubit) -> qubit {
  let q = X(q);
  let q = H(q);
  q
}

// only unitary quantum operations allowed, no  output qubits > input qubits
isometry fn f(qs: [qubit; 2]) -> [qubit; 4] {
  let q1 = X(qs[0]);
  let q2 = H(qs[1]);
  let q3 = qalloc();
  let q4 = qalloc();
  let q3 = X(q3);
  let (q3, q4) = CX(q3, q4);
  [q1, q2, q3, q4]
}

// only unitary quantum operations allowed, no  output qubits < input qubits
coisometry fn f(qs: [qubit; 3]) -> [qubit; 2] {
  let q0 = X(qs[0]);
  let q1 = H(qs[1]);
  discard(qs[2]);
  [q0, q1]
}

// may include measurements, reset, discard quantum operations
general fn sample(q: qubit) -> bit {
  measr(q)
}

/////////////////////////
// (52) Integer literals
/////////////////////////

let x = 1000;
let x = 1_000;
let x = 1_000_000;

let h = 0xff;
let h = 0xFF;
let h = 0xff_u8;

let o = 0o77;
let o = 0o70_i16;

let b = 0b1010;
let b = 0b1111_0000;
let b = 0b1111_1111_1001_0000i64;

let z = 10u32;
let z = 10_u32;
let z = 123i32;
let z = 123_i32;

////////////////////////////////
// (53) Floating point literals
////////////////////////////////

let x = 1.0;
let x = 1.;
let x = 0.1;
let x = 1.0e-3;
let x = 12E+99;
let x = 12E+99_f64;

let f = 1.0f64;
let f = 1.0_f64;
let f = 0.1f32;
let f = 5f32; 

///////////////////////////////////////
// (54) Byte and bytes string literals
///////////////////////////////////////

let b = b'a';
let b = b'\n';
let b = b'\x41';

let bs = b"hello";
let bs = b"ABC\x41";


////////////////////////////////////////////////////
// (55) Rust style enums containing classical data:
////////////////////////////////////////////////////

// Unit-like enums:
enum ResultBit {
  Zero,
  One,
}

// usage of unit-like enums:
let r = ResultBit::Zero;

// Tuple-like enums:
enum Data {
    Left(i32),
    Right(i32, i32),
}

// usage of tuple-like enums:
let x = Data::Left(q0);
let y = Data::Right(q1, q2);

//  Struct-like enums:
enum Message {
    Move { x: i32, y: i32 },
    Write { text: String },
}

// usage of struct-like enums:
let msg = Message::Move { x: 10, y: 20 };

// important note: like Rust enums can mix unit-like, tuple-like and struct-like variants in the same enum declaration, but for brevity we only show one variant of each kind in the examples above.

//////////////////////////////////////////////////
// (56) Rust style enums containing quantum data:
//////////////////////////////////////////////////

// Leaf qenum, only tuple-like qenums are supported for quantum data:
qenum Data {

    Left(qubit),
    Right(qubit, qubit),
}

// usage of qenums:
let x = Data::Left(q0);
let y = Data::Right(q1, q2);

///////////////////////////
// (57) Rust style structs
///////////////////////////

struct Point {
  x: f64,
  y: f64,
}

let p = Point { x: 1.0, y: 2.0 };
let x = p.x;

// destructuring a struct:
struct Pair {
    q0: qubit,
    q1: qubit,
}
let mypair = Pair { q0, q1 };
let Pair { q0: q3, q1: q4 } = mypair;

// like in Rust structs can have methods:
struct Person {
    age: u32,
}

impl Person {
    fn new(age: u32) -> Person {
        Person {
            age,
        }
    }

    fn is_adult(&self) -> bool {
        self.age >= 18
    }
}

// creating a new instance of the struct and calling a method on it:
let person = Person::new(30);
let is_adult = person.is_adult();

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// (58) Quantum Contracts Function Clauses: requires, ensures + clean, stabilized, basis, separable, isolated, product
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// These are optional code annotations for functions that specify pre- & post-conditions that the quantum data should satisfy:
// they may be used as: requires, ensures or both requires and ensures clauses, and they may be used in any combination with the function effect qualifiers
// from as well as with each other. They must be placed after the function signature and before the function body.
// either requires or ensures clauses may be used, both of them or neither as these are optional annotations.
// There can be multiple requires or ensures clauses for a function, and they can be used in any combination with each other.
//
// - clean(qs) - the qubit(s) are all in $|0\rangle$ state and separated from the rest of qubits in the program.
// - basis([q1, q2, q3], X*Y*Z) - the qubit(s) are in an eigenstate of Pauli string operator like X*Y*Z and separated from the rest of qubits in the program.
// - separable(qs) - these qubits are in a separable state meaning that they are not entangled among and separated from the rest of qubits in the program.
// - isolated(qs) - these qubits are not entangled with the rest of qubits in the program even if possibly entangled among them.
// - stabilized(qs) - these qubits are in a state which is stabilized by the supplied operators and at the same time they are separated from the rest of qubits in the program.
// - product(qs, qs') - these qubit sets are not mutually entangled (their joint state in the program is a product state) but in each set qubits may be entangled among each other and may be entangled to other unspecified qubits in the program.

// Clean, stabilized, basis, separable, isolated are all unary predicates:
clean(q1)
basis(q1, X)
basis(q1, Y)
basis(q1, Z)
separable(qs)
isolated(q1)

// For unary predicated, when we need multiple qubits as arguments we can specify them as an array of qubits:
clean([q1, q2])

// second argument is a Pauli string:
basis([q1, q2], X*X)

separable([q1, q2])
isolated([q1, q2])

// stabilized, takes as second argument as a list signed Pauli strings:
stabilized(qs, [ +Z*Id, -Z*Z ])

// product takes multiple arguments which are either qubits or arrays of qubits:
product(q1, q2, qs)

// products can take multiple arrays of qubits as arguments:
product([q1, q2], [q3, q4], qs)

fn oracle(x: qubit, ancillas: [qubit; 3])
  requires clean(ancillas)
  ensures clean(ancillas) {
    // some code here
}

fn oracle(q1: qubit, q2: qubit, qs: [qubit; 2])
  requires clean(q1)
  requires isolated(qs)
  ensures product([q1, q2, qs]){
    // some code here
}

// stabilizer simple examples
requires stabilized(q, [ -Z ])

// more complex stabilizer example with multiple qubits and multi-term stabilizers:
fn make_ghz(q0: &qubit, q1: &qubit, q2: &qubit)
  requires clean([q0, q1, q2])
  ensures stabilized([q0, q1, q2], [+X*X*X, +Z*Z*Id, +Id*Z*Z]])
{
    H(q0);
    CNOT(q0, q1);
    CNOT(q0, q2);
}

The following gates can appear in statbilizer expressions: Id, X, Y, Z, H, S, SDG, SX, SXDG, T, TDG

The "requires" clauses specify the pre-conditions that must hold on the quantum data before the function is called, while the "ensures" clauses specify the post-conditions that must hold on the quantum data after the function returns ao "requires" clauses should precede "ensures" clauses in function signature.

///////////////////////////////////////////////////////////////
// (59) Using function as arguments to higher-order functions:
///////////////////////////////////////////////////////////////
unitary fn phase_kickback(
    qs: [qubit; 4],
    oracle: unitary fn(qs: [qubit; 4], target: qubit) -> ([qubit; 4], qubit)
) -> [qubit; 4] {
    let scratch ancilla = qalloc();
    let ancilla = prepare_minus(ancilla);
    let (qs, ancilla) = oracle(qs, ancilla);
    qs
}

//////////////////////////////////////////////////////////
// (60) Declaring adjoint/controll support for functions:
//////////////////////////////////////////////////////////

// Leaf has a special syntax for those functions where the compiler is able to infer that the function is invertible or controllable using the "supports" keyword combined with "adjoint" or "ctrl" keywords respectively.

unitary fn f(q: qubit) supports adjoint {
    H(&q);
}

unitary fn f(q: qubit) supports ctrl {
    H(&q);
}

unitary fn f(q: qubit) supports adjoint, ctrl {
    H(&q);
}

//////////////////////////////////////////////////////////////////////
// (61) Combining function qualifiers, contracts and support clauses:
//////////////////////////////////////////////////////////////////////

unitary fn f(q1: qubit, q2: qubit, qs: [qubit; 2])
    supports adjoint, ctrl
    requires clean(q1)
    ensures basis(q2)
    requires isolated(qs)
    ensures product(q1, q2, qs) {
    H(&q);
}

///////////////////////////////////////////////
// (62) Combining qenums with quantum qmatch:
///////////////////////////////////////////////

qenum Data {
    Left(qubit),
    Right(qubit, qubit),
}

unitary fn transform(x: Data) -> Data {
    qmatch x {
        Data::Left(a) => Data::Left(H(a)),

        Data::Right(b, c) => {
            let (b, c) = CNOT(b, c);
            Data::Right(b, c)
        }
    }
}

////////////////////////////////////////////////////////
// (63) Phase function for representing complex phases:
////////////////////////////////////////////////////////

// the phase function can take as arguments a floating
phase(1.5)

// the phase function can take as arguments an angle
let angle: angle64 = 1.88;
phase(angle)

///////////////////////////////////////////////////////////
// (64) Like in Rust function arguments can be references:
///////////////////////////////////////////////////////////

fn apply_gate(q: &qubit)
{
  H(q);
}

fn main() -> bit {
  let q = qalloc();
  apply_gate(&q);
  let b = measr(q);
  b
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// (65) Top level expressions must be items: functions, structs, enums, impl blocks, use statements, consts:
////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Like in Rust at the top level Leaf expects items:

fn main() {
}

struct Person {
    height: i32,
    age: i32,
}

enum Color {
    Red,
    Green,
    Blue,
}

const I: i32 = 1;

impl Person {
    fn new(height: i32, age: i32) -> Person {
        Person { height, age }
    }
}

use my_library::helper;

// Top-level code must be made of items, not normal executable statement. These are not allowed at the top level:
let i = 1;
let p = Person { height: 10, age: 20 };

/////////////////////////////////////////////////////////////////////////////
// (66) Function annotations (only qasm_gate/qasm_def is supported for now):
/////////////////////////////////////////////////////////////////////////////

// this function is compiled to a OpenQASM3 subroutine representing an unitary operation
#[qasm_gate]
unitary fn myfun(q: qubit) -> qubit {
  let q = X(q);
  let q = H(q);
  q
}

// annotating the function with a string argument
#[qasm_gate("qasm_subroutine_name")]
unitary fn myfun(q: qubit) -> qubit {
  let q = X(q);
  let q = H(q);
  q
}

// this function is compiled to a named QASM subroutine with same signature
#[qasm_def]
unitary fn myfun(q: qubit) -> bit {
  let q = X(q);
  let q = H(q);
  measr(q)
}

// annotating the function with a string argument
#[qasm_def("qasm_subroutine_name")]
unitary fn myfun(q: qubit) -> bit {
  let q = X(q);
  let q = H(q);
  measr(q)
}