### Qubit type annotations

Linear qubits ('linear') must be used exactly once, no copying or discarding allowed. This is the default, so the 'linear' keyword is optional.
```leaf
let linear q: qubit = qalloc();
let linear qs: [qubit; 2] = qalloc(2);
```

Affine qubits ('affine') must be used at most once, no copying allowed, but discarding is allowed.
```leaf
let affine q: qubit = qalloc();
let affine qs: [qubit; 2] = qalloc(2);
```

Scratch qubits ('scratch') are automatically uncomputed at the end of their scope.
```leaf
let scratch q: qubit = qalloc();
let scratch qs: [qubit; 2] = qalloc(2);
```

Obviously linear or affine qubit type qualifiers can be combined with scratch qubits.
```leaf
let scratch linear q: qubit = qalloc();
let scratch linear qs: [qubit; 2] = qalloc(2);
let scratch affine q: qubit = qalloc();
let scratch affine qs: [qubit; 2] = qalloc(2);
```
