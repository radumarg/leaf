
### Quantum Conditionals and Generalization

Quantum conditionals are the first step in an attempt to go beyond the *quantum data + classical control* paradigm of quantum programming [[Selinger (2004)]](./bibliography.md#selinger-2004-towards-quantum-programming-language) which is based on the QRAM model of a quantum computation [[Knill (1996)]](./bibliography.md#knill-1996-conventions-quantum-pseudocode). The path from here toward more general quantum control over quantum data will probably be an interesting journey.

#### Resource-oriented qubits model: `qif`/`qelse`/`qmatch`

For a [resource oriented](qubit-representations.md#resource-oriented-qubit-model) qubits model, a quantum conditional on qubit q means applying two quantum operations on some other set of qubits depending on the state of `q` coherently without measuring it. The precise semantics of this construction is discussed [here](defining-terms.md#what-are-quantum-conditionals).

```leaf
fn f1(q1 : qubit, q2 : qubit) -> (qubit, qubit) { ... }
fn f2(q1 : qubit, q2 : qubit) -> (qubit, qubit) { ... }

let (q1, q2, q3) = qif q1 {
  f1(q2, q3)
} qelse {
  f2(q2, q3)
}
```

Besides expressions, statements are also supported in the qif/qelse branches:

```leaf
fn f3(q1 : qubit, q2 : qubit) { ... }
fn f4(q1 : qubit, q2 : qubit) { ... }

qif &q1 {
  f3(&q2, &q3);
} qelse {
  f4(&q2, &q3);
}
```

It is required of f1 and f2 to be unitary functions (no discarding on input qubits, no measurements or resets), to operate on the same number of qubits, and not act on the control qubit. Any ancilla qubits created inside the two functions must be returned in a clean pure zero state and in the end safely discarded.

A generalization of quantum conditional for multiple branches implies coherent control over qubits `qs` without performing measurements:

```leaf
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
```

  Assuming branching is done in computational basis states, the following syntax is also supported:

```leaf
  qmatch &qs {
    0 => f00(&q1, &q2, &q3),
    1 => f01(&q1, &q2, &q3),
    2 => f10(&q1, &q2, &q3),
    3 => f11(&q1, &q2, &q3),
  }

  // which is the same as:

  qmatch &qs {
    bs"00" => f00(&q1, &q2, &q3),
    bs"01" => f01(&q1, &q2, &q3),
    bs"10" => f10(&q1, &q2, &q3),
    bs"11" => f11(&q1, &q2, &q3),
  }
```

Similar conditions that apply to quantum conditionals branches apply here as well for functions in `qmatch` branches. Like in Rust, match must be exhaustive and `_ => ` is supported.

#### (2) State-oriented qubits model: `sif`/`selse`/`smatch`

For a [state oriented](qubit-representations.md#state-oriented-qubit-model) qubits model the elementary quantum conditional example is the X gate represented as a coherent operation via:

```leaf
fn not(q: qubit) -> qubit {
    sif q then zero selse one
}
```

```leaf
fn cnot(q: qubit) -> qubit {
    sif q then
        zero + one
    selse
        zero - phase(PI / 2) * one
}
```

The whole function maps a qubit to a qubit and denotes a transformation on q. To grasp this more easily it is useful to realize that `zero` could have been named: `qfalse` and `one`: `qtrue`, following the notation from [Grattage (2008)](./bibliography.md#grattage-2008-overview-qml-haskell). The function returns a symbolic state expression which cannot contain quantum gates or non-unitary qubit operations like measure, reset or discard. In order for the operation to describe a unitary transformation the `sif`/`selse` branches must be provably orthogonal which in the example above is obvious.
