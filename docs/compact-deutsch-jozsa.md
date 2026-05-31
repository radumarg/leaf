### Deutsch-Jozsa Algorithm

This is a more compact implementation of Deutsch-Jozsa:

```rust
unitary fn balanced_oracle(qs: [qubit; 5]) -> [qubit; 5] {
    // Negative controls on qs[0] and qs[2] by X-conjugation.
    X(&qs[0]);
    X(&qs[2]);

    for i in 0..4 {
        ctrl(&qs[i]) {
            X(&qs[4]);
        }
    }

    // Restore qs[0] and qs[2].
    X(&qs[0]);
    X(&qs[2]);

    qs
}

general fn deutsch_jozsa_balanced() -> [bit; 4] {
    let qs = qalloc(5);

    // Prepare ancilla in |1>.
    X(&qs[4]);

    // Apply H to input register and ancilla.
    for i in 0..5 {
        H(&qs[i]);
    }

    let qs = balanced_oracle(qs);

    // Interference step on the input register only.
    for i in 0..4 {
        H(&qs[i]);
    }

    let bs = measr(qs[0], qs[1], qs[2], qs[3]);

    // qs[4] is not part of the Deutsch-Jozsa result.
    discard(qs[4]);

    bs
}

general fn main() -> [bit; 4] {
    deutsch_jozsa_balanced()
}
```
