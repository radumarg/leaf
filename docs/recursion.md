### Recursion

Recursion does introduce a layer of complexity in a programming language and has consequences on what a programming language can and cannot do. For example the [principle of deferred measurement](defining-terms.md#what-is-principle-of-deferred-measurement) no longer applies automatically in the presence of recursion. A useful method classify the effect of recursion is using function effects:

(1) Purely classical recursion.
```leaf
classical fn fact(n: i32) -> i32 {
    if n == 0 { 1 } else { n * fact(n - 1) }
}
```

(2) Circuit-generating unitary recursion over a classical parameter, safe if total and structurally decreasing.
```leaf
unitary fn apply_hadamard_layer(n: i32, qs: [qubit]) {
    if n == 0 {
        return;
    } else {
        H(&qs[n - 1]);
        apply_hadamard_layer(n - 1, qs);
    }
}
```

(3) Classically controlled recursion. This can be mapped to OpenQasm3 recursive calls with mid-circuit measurements.
```leaf
general fn repeat_until_zero(q1: qubit) {
    let b = measure(q);

    if b == 0 {
        return;
    } else {
        let q2 = qalloc()

        H(&q2);

        repeat_until_zero(&q2);

        CX(&q2, &q1);

        discard(q2);
        return;
    }
}
```

(4) Quantum controlled recursion.

Not yet supported.

(5) Recursive quantum types.

Not yet supported.
