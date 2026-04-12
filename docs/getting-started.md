
### Starting point:

Leaf is deliberately designed to replicate Rust’s basic syntax, with minimal extensions for quantum programming. It follows Rust’s philosophy of strong static type support and extends it to quantum operations.

### The lifecycle of Leaf code:

- Frontend: Rust-like surface syntax.
- Type checker: Silq-inspired typing for quantum programming, optional automatic uncomputation.
- First compiler pass: translation to a typed high-level IR.
- High-level IR: based on lambda calculus/quantum lambda calculus.
- Second compiler pass: translation to the Idris2 DSL.
- Idris2 DSL: low-level IR.
- Third compiler pass: serialization of the low-level IR.
- Backend target: OpenQASM 3 for now, with QIR to be added later.

### Leaf will provide strong support for formal verification:

- Type safety ensured by a strong static type system at both the surface-syntax level and the typed high-level IR, with extensive coverage of quantum operations and their constraints.
- Semantic correctness of the algorithm implementation: proved over the high-level IR representation using Lean.
- Correctness of language lowering: the translations to the Idris DSL and subsequently to OpenQASM3/QIR preserve the semantics of the high-level IR.
- Idris2 DSL (low-level IR) is equipped with correctness guarantees ensuring that qubit indices are valid, control qubits are distinct from target qubits, and all control/target qubits are pairwise distinct when multiple qubits are involved.