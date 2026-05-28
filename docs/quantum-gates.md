## Quantum Gates


Example gate applications:
```leaf
let q: qubit = H(q);

// or:

let q = H(q);
```

For a two qubit gate:

```leaf
let (q0, q1) : (qubit, qubit) = CX(q0, q1);

// or:

let (q1, q2) = CNOT(q1, q2);
```

These operations are built in, but conceptually they can also act like functions that borrow their qubit arguments:

```leaf
H(&q);
CNOT(&q1, &q2);
```

Since qubits are mutable by default so the `H(&q)` syntax accepted by Leaf would have been written in Rust as: `H(&mut q)`.

### Identity

```leaf
Id(&q);
```

### Single-Qubit Gates

```leaf
X(&q);
Y(&q);
Z(&q);
H(&q);
S(&q);
SDG(&q);
SX(&q);
SXDG(&q);
T(&q);
TDG(&q);
```

### Parametric Single-Qubit Gates

```leaf
RX(1,0, &q);
RY(1.0, &q);
RZ(1.0, &q);
U1(1.0, &q);
U2(1.0, 2.0, &q);
U3(1.0, 2.0, 3.0, &q);
```

### Controlled Gates

```leaf
CX/CNOT(&q1, &q2);
CY(&q1, &q2);
CZ(&q1, &q2);
CS(&q1, &q2);
CSDG(&q1, &q2);
CSX(&q1, &q2);
CSXDG(&q1, &q2);
CT(&q1, &q2);
CTDG(&q1, &q2);
CRX(&q1, &q2);
CRY(&q1, &q2);
CRZ(&q1, &q2);
CU1(&q1, &q2);
CU2(&q1, &q2);
CU3(&q1, &q2);
```

### Two-Qubit Interaction Gates

```leaf
SWAP(&q1, &q2);
RXX(1,0, &q1, &q2);
RYY(1,0, &q1, &q2);
RZZ(1,0, &q1, &q2);
```

### Three-Qubit Gates

```leaf
CCX(&q1, &q2, &q3);
CSWAP(&q1, &q2, &q3);
```

### IonQ-Native Gates

```leaf
GPI(1.0, &q);
GPI2(1.0, &q);
MS(1.0, 2.0, &q1, &q2);
ZZ(1.0, &q1, &q2);
```

### Barrier
Barrier is a built-in function not a quantum gate and is compiled directly to an OpenQasm3 barrier instruction:
```leaf
barrier();
barrier(&q1, &q2);
```
