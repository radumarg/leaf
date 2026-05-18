### Modeling Qubits

- Resource-oriented model — qubits are treated as mutable computational resources whose quantum state evolves when quantum gates are applied.

```leaf
let q: qubit = qalloc();
let q = H(q);
```

- State-oriented model — qubits are represented using a language of expressions denoting vectors or states in a Hilbert space:

```leaf
fn qnot(q: qubit) -> qubit {
    sif q then zero selse one
}
```

Here `zero`, `one`, `plus` and `minus` are `squbit` typed language constants:

```leaf
fn had(q: qubit) -> qubit {
    sif q then
        1/sqrt(2) * (zero - one)
    selse
        1/sqrt(2) * (zero + one)
}
```

same as:

```leaf
fn had(q: qubit) -> qubit {
    sif q then plus selse minus
}
```

Using `squbit` type, the Hadamard operation can be implemented as well as:

```leaf
let plusAlias : squbit = 1/sqrt(2) * (zero + one);
let minusAlias : squbit = 1/sqrt(2) * (zero - one);

fn had(q: qubit) -> qubit {
    sif q then minusAlias selse plusAlias
}
```

The state oriented representation of qubits establishes a denotational unitary expression fragment which useful for generating quantum programs starting from physics denotation instead of using on an abstract quantum circuit model. The prototypical example for this use case is generating a circuit for QFT from its mathematical definition within the Leaf programming language.
