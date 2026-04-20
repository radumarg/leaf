### Unitary Adjoint Operator

```leaf
adjoint f(q1, q2, q3);

// SAME AS:
(adjoint f)(q1, q2, q3);

// SAME AS:
adjoint {
    f(q1, q2, q3);
}
```

```leaf
adjoint H(q1);
adjoint CX(q1, q2)

// SAME AS:
(adjoint H)(q1);
(adjoint CX)(q1, q2)

// SAME AS:
adjoint {
    H(q1);
    CX(q1, q2)
}

```