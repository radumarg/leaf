### Sum and Product Data Types

Like Rust, Leaf supports sum data types (Enum) and product data types (Tuple & Struct).

- Enums cannot contain quantum data:

```leaf
enum Direction {
    Up,
    Down,
    Left,
    Right,
}
```

- Tuples and structs can contain quantum data but if they do become linear types:
  
```leaf
let qubits = CNOT(q0, q1);
let q0 = qubits.0;
let q1 = qubits.1;

// accesing tuple qubits after qubits have been moved out is illegal:
let qubitsCopy = qubits; 
```

```leaf
struct Pair {
    q0: qubit,
    q1: qubit,
}

// forgeting to access pair.q1 means implicit qubit discard which is illegal:
{
    let q0 : qubit = qalloc();
    let q1 : qubit = qalloc();
    let pair = Pair { q0, q1 };
    let q0 = H(pair.q0);
    let b0 = measr(q0); 
}

// all qubits have been consumed:
{
    let q0: qubit = qalloc();
    let q1: qubit = qalloc();
    let pair = Pair { q0, q1 };
    let Pair { q0: q2, q1: q3 } = pair;
    discard(q2, q3);
}
```