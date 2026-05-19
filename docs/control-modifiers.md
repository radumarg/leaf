### Control gate modifiers: `ctrl` and `negctrl`

Technically these are higher-order operators that change the way functions with quantum operations behave. As expected, `negctrl` triggers when the control qubit is 0, whereas the standard `ctrl` triggers when it is 1.

Canonical declaration:
```leaf
let (q0, q1, q2): (qubit, qubit, qubit) = ctrl(q0, q1).H(q2);
let (q0, q1, q2): (qubit, qubit, qubit) = negctrl(q0, q1).H(q2);
```

`ctrl` and `negctrl` can be chained
```leaf
let (q0, q1, q2) = ctrl(q0).negctrl(q1).X(q2);
```

Controlled gate with inferred types:
```leaf
let (q0, q1, q2) = ctrl(q0, q1).H(q2);
let (q0, q1, q2) = negctrl(q0, q1).H(q2);
```

Controlled gate with borrowed qubits such that qubits can be reused in a subsequent operation:
```leaf
ctrl(&q0, &q1).H(&q2);
negctrl(&q0, &q1).H(&q2);
```

Chain ctrl and negctrl modifiers:
```leaf
let (q0, q1, q2) = ctrl(q0).negctrl(q1).H(q2);
let (q0, q1, q2) = ctrl(q0).ctrl(q1).H(q2);
```

Apply control on 0/1 states:
```leaf
let (q0, q1, q2) = ctrl(q0 : one, q1 : zero).H(q2);
```
Apply control on +/- states:
```leaf
let (q0, q1, q2) = ctrl(q0 : minus, q1 : plus).H(q2);
```

Block syntax:
```leaf
ctrl(&q0 : one, &q1 : one, &q2 : zero) {
  H(&q3);
  CX(&q4, &q5);
};
```

```leaf
(zero + one)
 .tensor(zero - phase(pi/2) * one)
 .tensor(zero - one)
```