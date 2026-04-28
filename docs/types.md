### Leaf basic types

(1) Quantum computing specific types:
```leaf
bit, qubit, squbit
```

where the `qubit` type is used for regular qubits, that accept gate(s) application:
```leaf
let q : qubit = H(q); 
```

while the `squbit` is used for expression that declare quantum states like this:

```leaf
let plusAlias : squbit = 1/sqrt(2) * (zero + one)
let oneAlias  : squbit = 1/sqrt(2) * (plus - minus)
```

a `squbit` can be cast to a `qubit` using the synthesize built-in:
```leaf
let q : qubit = synth(sq); 
```

(2) Angle types: 32-bit or 64-bit floating-point values in the range [0, 2π)
```leaf
angle32, angle64
```

(3) Param type:
```leaf
let theta: param = Param("theta");
```

(4) Signed integer types:
```leaf
i8, i16, i32, i64, i128
```

(5) Unsigned integer types:
```leaf
u8, u16, u32, u64, u128
```

(6) Floating-point types:
```leaf
f32, f64
```

(7) Boolean type:
```leaf
bool
```

(8) Unit type:
```leaf
let () = ();
```
