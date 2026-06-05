### Sum and Product Data Types

Like Rust, Leaf supports sum data types (Enum) and product data types (Tuple & Struct). When containing quantum data:
- product types correspond to tensor products of Hilbert spaces.
- sum types correspond to direct sums of Hilbert spaces.

#### Product Data Types

Tuples and structs can contain quantum data but if they do, they become linear types.

*Example Tuple Usage*

  
```leaf
let qubits = CNOT(q0, q1);
let q0 = qubits.0;
let q1 = qubits.1;

// accesing tuple qubits after qubits have been moved out is illegal:

let qubitsCopy = qubits;
```

*Example Struct Usage*

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

// all qubits have been consumed in this case:
{
    let q0: qubit = qalloc();
    let q1: qubit = qalloc();
    let pair = Pair { q0, q1 };
    let Pair { q0: q2, q1: q3 } = pair;
    discard(q2, q3);
}
```

#### Sum Data Types

Sum types should be understood as a method to specify a number of distinct alternatives for some data. While supported in Leaf, Rust style Enums cannot contain quantum data:

```leaf
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

// or:

enum Message {
    Move { x: i32, y: i32 },
    Size { s: i32 },
}
```

On the other hand, Leaf has a special syntax for Enums that contain quantum data:

```leaf
qenum Data {
    Left(qubit),
    Right(qubit, qubit),
}

// usage example:

unitary fn transform_data(x: Data) -> Data {
    qmatch x {
        Data::Left(q) => Data::Left(H(q)),

        Data::Right(q1, q2) => {
            let (q1, q2) = CNOT(q1, q2);
            Data::Right(q1, q2)
        }
    }
}
```

For a programmer, the `qenum` + `qmatch` syntax permits quantum branching. The example above demonstrates an operator $H \oplus \mathrm{CNOT}$ acting coherently on: $\mathbb{C}^2 \oplus \mathbb{C}^4$.

More generally if we have two functions:

```leaf
fn f (q: qubit) -> qubit { ... }
fn g (q1: qubit, q2: qubit) -> (qubit, qubit) { ... }
```

acting on a `qenum`:

```leaf
qenum Data {
    Left(qubit),
    Right(qubit, qubit),
}
```

like this:

```leaf
qmatch x {
    Left(a) => Left(f(a)),
    Right(b,c) => Right(g(b,c)),
}
```

will denote something like f ⊕ g. If the input is:

```leaf
α · Left(a) + β · Right(b)
```

then the result is:

```leaf
α · Left(f(a)) + β · Right(g(b))
```

Like in most circuit languages, everything is ultimately encoded into a register of qubits. So the six dimensional space `qubit + (qubit × qubit)` will be embedded into a register of 3 qubits corresponding to an eight dimensional Hilbert space with some unused states. This construction allows the programmer to describe algorithms over subspaces, not just registers and think on a level which is higher than the raw qubit representation of languages like OpenQasm3. For most near-term algorithms this in not very useful, however this construct becomes relevant for algorithms based on: quantum walks, automata, graph states, symbolic states, subspace algorithms.
