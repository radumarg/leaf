### Operations on Qubits

- Allocating qubits (output type can be inferred):
```leaf
let q: qubit = qalloc();
let qs: [qubit; 2] = qalloc(2);
```

- Measuring qubits (result type can be inferred):
```leaf
let b : bit = measr(q);
let (b1 : bit, b2 : bit, b3 : bit) = measr(q1, q2, q3);
let bs = measr(qs);
```

- Downgrade qubit type from `linear` to `affine`:
```leaf
weaken(q);
weaken(q1, q2, q3);
weaken(qs);
```

- Reseting qubits:
```leaf
reset(q);
reset(q1, q2, q3);
reset(qs);
```

- Discarding qubits:
```leaf
discard(q);
discard(q1, q2, q3);
discard(qs);
```

- Automatic [uncomputation](defining-terms.md#what-does-uncomputation-mean), works only over circuits generated with `uncompsafe` functions:
```leaf
uncompute(q);
uncompute(q1, q2, q3);
uncompute(qs);
```

- Compose two state expressions horizontally - $|00\rangle$:  
```leaf
let sq1 : squbit = zero
let sq2 : squbit = zero
let sq : squbit = sq1.then(sq2);
```

- Compose two state expressions vertically - $|1\rangle \otimes |1\rangle$:
```leaf
let sq1 : squbit = one
let sq2 : squbit = one
let sq : [squbit; 2] = sq1.tensor(sq2);
```

- Trigger circuit synthesis by casting `squbit` to `qubit`:
```leaf
let q : qubit = synth(sq);
let qs : [qubit; 2] = synth(sqs);
```