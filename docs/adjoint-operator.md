### Unitary Adjoint Operator

Technically these are higher-order operators that change the way functions with quantum operations behave. Adjoint is can be only applied to unitary code, in particular if applied to a function the function must be qualified with the `unitary` effect.

```leaf
adjoint f(q1, q2, q3);

// SAME AS:

adjoint {
    f(q1, q2, q3);
}
```

```leaf
adjoint {
    H(&q1);
    CX(&q1, &q2)
}

// SAME AS:

adjoint CX(&q1, &q2)
adjoint H(&q1);
```

### Reversing quantum subroutines

When can the action function be reversed? The safest answer is that it can be reversed when it contains only unitary operations on qubits: no resets, not measurements no discarding of qubits. However, there are conditions when these requirements can be relaxed.

Discarding a quantum register in code amounts allowing the physical qubits to dissipate. In general dissipating a register means its coherent information is lost to the environment, with possible side effects on the remaining quantum state and this is in general not a reversible operation. However, when discarded register is known to be in a clean (separable, all zero) state this operation becomes harmless. If the discarded qubits are reinitialized to the all zero, this can be written formally as:

 $\rho_{AB} = Tr_{B}(\rho_{AB}) \otimes  \lvert 0 \rangle  \langle 0 \rvert_{B}$

Under such conditions a subroutine that contain no measurement, no side effects on classical inputs, and no classical output, can be in principle reversed.
