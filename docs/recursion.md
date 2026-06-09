### Recursion

(1) Purely classical recursion:
```leaf
classical fn fact(n: i32) -> i32 {
    if n == 0 { 1 } else { n * fact(n - 1) }
}
```

(2) Circuit-generating unitary recursion over a classical parameter, safe if total and structurally decreasing:
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

(3) Classically controlled recursion. This can be mapped to OpenQasm3 recursive calls. Allowed, but not adjointable or controllable by default:
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

(4) Quantum controlled recursion:

Not yet supported.

(5) Recursive quantum types:

Not yet supported.
