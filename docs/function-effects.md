### Function Effects

Function effects are annotations used by the Lean type checker to verify Leaf code. The function effects form a lattice:

```leaf
classical < uncompsafe < unitary < general
```

- `classical` is the default effect used to classify strictly classical functions i.e. function that do not have qubit arguments, do not return qubits and do not perform any quantum operations like qubit allocation or operations on quantum data. Being the default effect, the `classical` keyword is optional and is mainly used for generating explicit API specification:

```leaf
classical fn parity (x : u32) -> bool { ... }
```

- Automatic [uncomputation](defining-terms.md#what-does-uncomputation-mean) is possible when the computation of the temporary value can be described classically. The `uncompsafe` effect is used to classify functions containing a subset of strictly unitary quantum operations that do not generate or destroy entanglement. These basis-preserving quantum gates are used to generate circuits whose effects can be undone automatically such that the ancilla qubits can be subsequently discarded safely:

```leaf
uncompsafe fn oracle (ancillas : [qubit; 3]) -> [qubit; 3] { ... }
```

- `unitary` is used to classify function containing unitary quantum gates or invoking `unitary` functions:
 
```leaf
unitary fn grover (qubits : [qubit; 7]) -> [qubit; 7] { ... }
```

- `general` is used to classify functions which in addition to quantum gates contain `measr`, `reset` or `discard` operations or are invoking `general` functions.

```leaf
general fn sample (qs : [qubit; 7]) -> [bit; 7] { ... }
```




