### Deutsch-Jozsa Algorithm

```rust
unitary fn balanced_oracle(
    q0: qubit,
    q1: qubit,
    q2: qubit,
    q3: qubit,
    q4: qubit,
) -> (qubit, qubit, qubit, qubit, qubit) {

    // Negative controls on q0 and q2 by X-conjugation.
    X(&q0);
    X(&q2);

    ctrl(&q0) { X(&q4); }
    ctrl(&q1) { X(&q4); }
    ctrl(&q2) { X(&q4); }
    ctrl(&q3) { X(&q4); }

    // Restore q0 and q2.
    X(&q0);
    X(&q2);

    (q0, q1, q2, q3, q4)
}

general fn deutsch_jozsa_balanced() -> (bit, bit, bit, bit) {

    let q0 = qalloc();
    let q1 = qalloc();
    let q2 = qalloc();
    let q3 = qalloc();
    let q4 = qalloc();

    // Prepare ancilla in |1>.
    X(&q4);

    // Apply H to input register and ancilla.
    H(&q0);
    H(&q1);
    H(&q2);
    H(&q3);
    H(&q4);

    let (q0, q1, q2, q3, q4) =
        balanced_oracle(q0, q1, q2, q3, q4);

    // Interference step on the input register.
    H(&q0);
    H(&q1);
    H(&q2);
    H(&q3);

    let b0 = measr(q0);
    let b1 = measr(q1);
    let b2 = measr(q2);
    let b3 = measr(q3);
    
    // q4 is not part of the Deutsch-Jozsa result.
    discard(q4);

    (b0, b1, b2, b3)
}

general fn main() -> (bit, bit, bit, bit) {
    deutsch_jozsa_balanced()
}
```
