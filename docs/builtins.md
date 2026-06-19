### Built-in Functions


[Qubit operations](quantum-data-operations.md#operations-on-qubits):
```leaf
qalloc(), measr(), discard(), reset(), uncompute(), weaken(), tensor()
```

[Quantum contracts](quantum-contracts.md#quantum-contracts):
```leaf
clean(), basis(), isolated(), product(), separable(), stabilized()
```

[Circuit operations](quantum-gates.md#barrier):
```leaf
barrier()
```

[Adjoint operations](adjoint-operator.md):
```leaf
adjoint()
```

[Control operations](control-modifiers.md):
```leaf
ctrl().on().apply()
```

Complex-valued phase() prelude function is often used as helper for quantum states specification:
```leaf
phase(1.23) = exp(i * 1.23);
```

The turns() prelude function translates floating point numbers representing fractions of 2π into angle type values:
```leaf
turns(0.25) = π/2;
```

Declaring angle parameters via Param() built-in:
```leaf
Param()
```

Trigonometric prelude functions:
```leaf
cos(), acos(), sin(), asin(), tan(), atan()
```

Math utility prelude functions:
```leaf
abs(), exp(), ceil(), floor(), ln(), log2(), log10(), max(), min(), round(), sqrt()
```

