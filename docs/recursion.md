### Recursion

(1) Classical Leaf recursion in the compiler is easily handled:

```leaf
unitary fn apply_qft(circuit, qs) {
    ...
    apply_qft(circuit, tail);
    ...
}
```

(2) Classically controlled recursion:

```leaf
fn repeat_until_zero(q1: &qubit) general {
    let b = measure(q);

    if b == 0 {
        return;
    } else {
        let q2 = qalloc()

        H(&q2);

        repeat_until_zero(&q2);

        CX(&q2, q1);

        discard(q2);
        return;
    }
}
```

(3) Runtime loops and repeat-until-success

TODO

(4) Quantum recursion

TODO