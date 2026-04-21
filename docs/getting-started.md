
### Starting point:

Leaf is deliberately designed to replicate Rust’s basic syntax, with minimal extensions for quantum programming which are meant to look and feel like Rust. It follows Rust’s philosophy of strong static type support and extends it to quantum data (qubits) and quantum operations (unitary and non-unitary).

More precisely, Leaf is intended to be a statically typed, sound, and safe language. This means that types are checked by the compiler before execution. A sound type system ensures that well-typed programs come with formal guarantees that they behave according to the language’s semantics, without undefined behaviors, type-related execution errors, or attempts to perform non-physical quantum operations, enjoying ancilla qubits management and automatic uncomputation support. Safety ensures that bad runtime behaviors are ruled out by the type system or, where necessary, by runtime checks.

### The compilation lifecycle of Leaf code:

1. Frontend - Rust-like surface syntax.
2. Type checker - provides strong type safety for quantum programming with optional automatic uncomputation support.
3. First compiler pass - translation of the surface syntax to a typed, small core, high-level IR based on quantum lambda calculus.
4. Second compiler pass - translates high level IR code to an intermediate IR represented by an Idris2 DSL.
5. Third compiler pass - serialization of Idris2 DSL to a low-level IR.
6. Backend target - OpenQASM3 for now, QIR to be added later.

### Leaf will provide strong support for formal verification:

- Type safety is ensured by a strong static type system at both the surface-syntax level and the typed high-level IR, with rigorous coverage of quantum operations and their constraints.
- The developer will have the tools to test the semantic correctness of his/her the algorithm implementation over the high-level IR representation of the algorithm using Lean.
- Correctness of language lowering: the translations to the Idris DSL and subsequently to OpenQASM3/QIR should preserve the semantics of the high-level IR (denotational semantics preservation).
- The Idris2 DSL does not enforce a full typing discipline, but it is equipped with correctness guarantees expressed using Idris2’s dependent type system and builtin-in proof support, ensuring that qubit indices used in gate applications are valid, control qubits are distinct from target qubits, and all control/target qubits are pairwise distinct when multiple qubits are involved.