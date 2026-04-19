## Quantum Gates


Example gate applications:
```leaf
H(q);
U3(1.0, 2.0, 3.0, q);
CX(q1, q2);
```

These are built-in operations, but conceptually they borrow their qubit(s) argument(s) in order to avoid needing to write code like this: 
```leaf
let q = H(q);                   // INCORRECT SYNTAX
let q = U3(1.0, 2.0, 3.0, q);   // INCORRECT SYNTAX
let (q1, q2) = CX(q1, q2);      // INCORRECT SYNTAX
```

### Identity

```leaf
Id
```

### Single-Qubit Gates

```leaf
X(q);
Y(q);
Z(q);
H(q);
S(q);
SDG(q);
SX(q);
SXDG(q);
T(q);
TDG(q);
```

### Parametric Single-Qubit Gates

```leaf
RX(1,0, q);
RY(1.0, q);
RZ(1.0, q);
U1(1.0, q);
U2(1.0, 2.0, q);
U3(1.0, 2.0, 3.0, q);
```

### Controlled Gates

```leaf
CX/CNOT(q1, q2);
CY(q1, q2);
CZ(q1, q2);
CS(q1, q2);
CSDG(q1, q2);
CSX(q1, q2);
CSXDG(q1, q2);
CT(q1, q2);
CTDG(q1, q2);
CRX(q1, q2);
CRY(q1, q2);
CRZ(q1, q2);
CU1(q1, q2);
CU2(q1, q2);
CU3(q1, q2);
```

### Two-Qubit Interaction Gates

```leaf
SWAP(q1, q2);
RXX(1,0, q1, q2);
RYY(1,0, q1, q2);
RZZ(1,0, q1, q2);
```

### Three-Qubit Gates

```leaf
CCX(q1, q2, q3);
CSWAP(q1, q2, q3);
```

### Ion-Native Gates

```leaf
GPI(1.0, q);
GPI2(1.0, q);
MS(1.0, 2.0, q1, q2);
ZZ(1.0, q1, q2);
```

### Barrier
This translates directly to OpenQasm3 barrier instruction:
```leaf
barrier();
barrier(q1, q2);
```