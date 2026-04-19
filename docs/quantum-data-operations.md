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
```
Γ ⊢ q : linear qubit
----------------------------
Γ ⊢ weaken(q) : affine qubit
``` 

```leaf
weaken(q);
weaken(q1, q2, q3);
weaken(qs);
```

Type judgements for remaining operations:
```
Γ ⊢ q : qubit
--------------
Γ ⊢ op(q) : ()
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

- Automatic uncomputation: works only over circuits generated with `uncompsafe` functions:
```
uncompute(q);
uncompute(q1, q2, q3);
uncompute(qs);
```