### Control operators: `ctrl` and `negctrl`

Technically these are higher-order operators that change the way functions with quantum operations behave. As expected, `negctrl` triggers when the control qubit is 0, whereas the standard `ctrl` triggers when it is 1. These are built-in operations, but conceptually they borrow their qubit(s) argument(s) in order to avoid needing to write code like this:
```leaf
let (q0, q1) = ctrl(q0) H(q1);  // INCORRECT SYNTAX
```

Some valid code examples:

```leaf
ctrl(q0) H(q1);
negctrl(q0) H(q1);
ctrl(q0, q1) H(q2);
ctrl(q0) negctrl(q1) H(q2);

// SAME AS:
(ctrl(q0) H)(q1);
(negctrl(q0) H)(q1);
(ctrl(q0, q1) H)(q2);
(ctrl(q0) negctrl(q1) H)(q2);
```

Block syntax:

```leaf
ctrl(q0, q1) {
  H(q2);
  CX(q2, q3);
}
  
// SAME AS:
ctrl(q0, q1) H(q2);
ctrl(q0, q1) CX(q2, q3);
```