
### Quantum Conditionals and Quantum Match Statements

Quantum conditionals are the first step in an attempt to go beyond the [quantum data + classical control](https://www.mathstat.dal.ca/~selinger/papers/qpl.pdf) paradigm of quantum programming which was based on [QRAM model](https://www.osti.gov/servlets/purl/366453-CZpmV6/webviewable/) of a quantum computer. The road from here towards general quantum control over quantum data is probably going to be a long one.

#### (1) Resource-oriented qubits model: `qif`/`qelse`/`qmatch`

For a [resource oriented](qubit-representations.md#resource-oriented-qubit-model) qubits model, a quantum conditional on qubit q means applying two quantum operations on some other set of qubits depending on the state of `q` coherently without measuring it. The precise semantics of this construction is discussed [here](defining-terms.md#what-are-quantum-conditionals).

```leaf
fun f1(q1 : qubit, q2 : qubit) -> (qubit, qubit) { ... }
fun f2(q1 : qubit, q2 : qubit) -> (qubit, qubit) { ... }

let (q1, q2, q3) = qif q1 {
  f1(q2, q3)
} qelse {
  f2(q2, q3)
}
```

Besides expressions, statements are also supported in the qif/qelse branches:

```leaf
fun f3(q1 : qubit, q2 : qubit) { ... }
fun f4(q1 : qubit, q2 : qubit) { ... }

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

For a [state oriented](qubit-representations.md#state-oriented-qubit-model) qubits model the elementary quantum conditional example is the CNOT gate represented as a coherent operation via:

```leaf
fn qnot(q: qubit) -> qubit {
    sif q then zero selse one
}
```

```leaf
(zero + one)
 .tensor(zero - phase(pi/2) * one)
 .tensor(zero - one)
```

The whole function maps a qubit to a qubit and denotes a transformation on q. To grasp this more easily it is useful to realize that `zero` could have been named: `qfalse` and `one`: `qtrue`, following the notation from this [paper](https://arxiv.org/pdf/0806.2735). The function returns a symbolic state expression which cannot contain quantum gates or non-unitary qubit operations like measure, reset or discard. In order for the operation to describe a unitary transformation the `sif`/`selse` branches must be provably orthogonal which in the example above is obvious.
