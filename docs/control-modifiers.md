### Control Gate Modifiers

Technically these are higher-order operators that change the way functions with quantum operations behave. Only `uncompsafe`, `unitary`, `isometry` or `coisometry` functions can be controlled via using apply() construct.

Canonical declaration:
```leaf
let (q0, q1, q2) = ctrl(q0, q1).apply(H)(q2);
```

... or with qubit borrowing syntax:
```leaf
ctrl(&q0, &q1).apply(H)(&q2);
```

... or equivalently using block syntax:
```leaf
ctrl(&q0, &q1) {
  H(&q2);
}
```

Controlling both $|0\rangle$ and $|1\rangle$:
```leaf
let (q0, q1, q2) = ctrl(q0, q1).on(bs"01").apply(H)(q2);

// or:

ctrl(&q0, &q1).on(bs"01").apply(H)(&q2);

// or:

ctrl(&q0, &q1).on(bs"01") {
  H(&q2);
}
```

Applying controls on a generic function and in a different basis:
```leaf
let (q0, q1, q2, q3) = ctrl(q0, q1).on(bs"+-").apply(f)(q2, q3);

// or:

ctrl(&q0, &q1).on(bs"+-").apply(f)(&q2, &q3);

// or:

ctrl(&q0, &q1).on(bs"+-") {
  f(&q2, &q3);
}
```

### Declaring Controlling Support

A function that contains only unitary quantum gates always can be controlled. However, when the function has classical output, mutates classical data, generates side effects, measures, resets or discards qubits thing are no longer so simple. Sometimes quantum code can measure/reset/discard qubits and still be treated as unitary as long as those operations are applied to qubits that are in a provable clean, all zero, separable state. Leaf has a special syntax for those functions where the compiler is able to infer that the function can be quantum controlled using the `suppports` keyword:

```leaf
unitary fn f(q : qubit) supports ctrl {
    H(&q);
}
```