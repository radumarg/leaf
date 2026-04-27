
### Quantum Conditional Statement

A quantum conditional on qubit q means applying different quantum operations depending on the state of `q`, [coherently](defining-terms.md#what-are-quantum-conditionals) and without measuring it:

```leaf
qif q1 {
  f1(q2, q3);
} qelse {
  f2(q2, q3);
}
```

### Quantum Match Statements

A generalization of quantum conditional for multiple branches implies coherent control over qubits `qs` without performing measurements:

```leaf
  qmatch qs {
    b"00" => f00(q1, q2, q3);
    b"01" => f01(q1, q2, q3);
    b"10" => f10(q1, q2, q3);
    b"11" => f11(q1, q2, q3);
  }

  qmatch qs {
    0 => f00(q1, q2, q3);
    1 => f01(q1, q2, q3);
    2 => f10(q1, q2, q3);
    3 => f11(q1, q2, q3);
  }
```