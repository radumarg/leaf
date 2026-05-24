### Modeling Qubits

#### (1) Resource-oriented qubit model

Qubits are treated as mutable computational resources whose quantum state evolves when quantum gates are applied.

```leaf
let q: qubit = qalloc();
let q = H(q);
```

#### (2) State-oriented qubit model

Qubits are represented using a language of expressions denoting vectors or states in a Hilbert space built using the `squbit` typed language constants: `zero`, `one`, `plus` and `minus`. These constants are used inside state expressions and do not denote allocated runtime qubits.


```leaf
fn had(q: qubit) -> qubit {
    sif q then
        (zero - one)
    selse
        (zero + one)
}
```

Is the same as:

```leaf
fn had(q: qubit) -> qubit {
    sif q then minus selse plus
}
```

Using `squbit` type, the Hadamard operation can be implemented as well as:

```leaf
let plusAlias : squbit = zero + one;
let minusAlias : squbit = zero - one;

fn had(q: qubit) -> qubit {
    sif q then minusAlias selse plusAlias
}
```

Note that the overall normalization factor is ignored in state expressions. The state oriented representation of qubits establishes a denotational unitary expression fragment which useful among others for generating quantum programs starting from physics denotation instead of using on an abstract quantum circuit model. The prototypical example for this use case is generating a circuit for QFT from its mathematical definition within the Leaf programming language.
