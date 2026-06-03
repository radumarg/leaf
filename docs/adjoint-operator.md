
### Reversing Operations

Although this may be obvious, it is important to note that classical functions are not reversible in general. A classical function may map multiple inputs to the same output, thereby losing information needed to reconstruct the original input.

### Reversing Quantum Subroutines

When can the action function be reversed? The safest answer is that it can be reversed when it contains only unitary operations on qubits: no resets, not measurements no discarding of qubits. However, there are conditions when these requirements can be relaxed.

Discarding a quantum register in code amounts allowing the physical qubits to dissipate. In general dissipating a register means its coherent information is lost to the environment, with possible side effects on the remaining quantum state and this is in general not a reversible operation. However, when discarded register is known to be in a clean (separable, all zero) state this operation becomes harmless. If the discarded qubits are reinitialized to the all zero, this can be written formally as:

 $\rho_{AB} = Tr_{B}(\rho_{AB}) \otimes  \lvert 0 \rangle  \langle 0 \rvert_{B}$

Under such conditions a subroutine that contain no measurement, no side effects, and no classical output, no mutation of classical data, can be in principle reversed.

### The Adjoint Operation

Adjoint operation transformer turns a unitary call or block into its inverse operation. Technically these are higher-order operators that change the way functions with quantum operations behave. Adjoint is can be only applied to unitary code, in particular if applied to a function the function must be qualified with the `unitary` or `uncompsafe` effect.

Adjoint acting as a higher order function:

```leaf
let f_adjoint = adjoint(f);
```

Adjoint syntax with explicit qubit handling:

```leaf
let (q1, q2, q3) = adjoint(f)(q1, q2, q3);
```

... or using qubit borrowing syntax:

```leaf
adjoint(f)(&q1, &q2, &q3);
```

Adjoint block expression:

```leaf
adjoint {
    f(&q1, &q2, &q3);
}
```

A block expression with built-in gates:

```leaf
adjoint {
    H(&q1);
    CT(&q1, &q2)
}
```

can be decomposed as:

```leaf
adjoint(CT)(&q1, &q2);
adjoint(H)(&q1);
```

 ... which is the same with:

```leaf
CTDG(&q1, &q2);
H(&q1);
```

### Adjoint Operator vs Block Expression

The adjoint block expression has the effect to apply the adjoint all unitary gates within the block expression and sub-expressions after reversing the order of quantum gates application. It can be applied to blocks containing built-in unitary quantum gates and functions qualified with `classical`, `uncompsafe` or `unitary` effects. On the other hand, the adjoint function operator `adjoint(f)` can be applied only to functions that declare adjoint support in function signature (see below).

### Declaring Adjoint Support

Leaf has a special syntax for those functions where the compiler is able to infer that the function is invertible using the `suppports` keyword:

```leaf
unitary fn f(q: qubit) supports adjoint {
    H(&q);
}
```

Control and adjoint supports clauses can be combined:

```leaf
unitary fn f(q: qubit) supports adjoint, ctrl {
    H(&q);
}
```

A function that returns classical data or generates side effects cannot in general support adjoint operation since classical functions are not always invertible. A function that contains only unitary quantum gates always supports the adjoint operation and usually does not when it measures, resets or discards qubits. Sometimes quantum code can measure/reset/discard qubits and still be treated as unitary as long as those operations are applied to qubits that are provably driven to the all zero, separable state.
