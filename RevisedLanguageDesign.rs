////////////////////////////////
// Basic Leaf Language Syntax
////////////////////////////////

// Code lines are terminated by ';' like this is currently done in Rust except for function return syntax which does not end with ';', again like is the convention in Rust.

// Paranthesis, square brakets and curly braces follow the same rules from Rust.

///////////////////////////////////
// Reserved Keywords for Types:
///////////////////////////////////

// quantum computing specific:
bit, qubit

// additional quantum types (OpenQASM3 inspired):
angle32, angle64, Param

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
// Syntax for Basic Types
////////////////////////////////
let f:f32 = 1.234567;
let d:f64 = -1.2345678901234567;

// inferred types for floating point literals
let d = -1000.0;

let i:i8 = -1;
let i:i16 = -1;
let i:i32 = 1;
let i:i64 = 1;
let i:i128 = -1;

// inferred types for integer literals
let i = -7;

let q:qubit = qalloc();

// inferred type for qubit literals
let q = qalloc();

let u:u8 = 1;
let u:u16 = 1;
let u:u32 = 1;
let u:u64 = 1;
let u:u128 = 1;

let unit:() = ();

// inferred type for unit literal
let () = ();

////////////////////////////////
// Syntax for Parameters
////////////////////////////////
let theta : Param = param("theta")