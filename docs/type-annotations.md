### Qubit type annotations

Linear qubits must be used exactly once, no copying or implicit discarding allowed. Linear annotation is the default for qubits, so the 'linear' keyword is optional. Instead of implicit discarding, one should use the `discard()` method to mark qubits programmatically as discarded. However, you should be aware that discarding a qubit can have [physical consequences](quantum-data-operations.md#operations-on-qubits).
```leaf
let linear q: qubit = qalloc();
let linear qs: [qubit; 2] = qalloc(2);
```

Affine qubits must be used at most once, no copying allowed but implicit discarding is allowed. While using affine qubits may be useful in certain situations this should the exception rather than the rule.
```leaf
let affine q: qubit = qalloc();
let affine qs: [qubit; 2] = qalloc(2);
```

Scratch qubits are automatically uncomputed at the end of their scope and reclaimed automatically.
```leaf
let scratch q: qubit = qalloc();
let scratch qs: [qubit; 2] = qalloc(2);
```

Linear or affine qubit type qualifiers can be combined with scratch type qualifier.
```leaf
let scratch linear q: qubit = qalloc();
let scratch linear qs: [qubit; 2] = qalloc(2);
let scratch affine q: qubit = qalloc();
let scratch affine qs: [qubit; 2] = qalloc(2);
```
