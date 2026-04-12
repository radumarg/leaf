
By and large Leaf is on purpose replicating the Rust basic syntax with minimal extensions adding support for quantum computing programming.

The lifecycle of Leaf code:

 - Frontend (Rust like)
 - Type Checker (Silq Inspired)
 - High IR Translator (1st Compiler Pass)
 - Lambda Calculus / Quantum Lambda Calculus based typed IR
 - DSL Translation (2nd Compiler Pass)
 - Idris2 DSL
 - Low IR Serializer (3rd Compiler Pass)
 - OpenQasm3/QIR

Leaf will provide strong support for formal verification:

- Type safety ensured by a strong type system at both the surface-syntax level and the typed high-level IR, with extensive coverage of quantum operations and their constraints.
- Semantic correctness of the algorithm implementation: proved over the high-level IR representation using Lean.
- Correctness of language lowering: the translations to the Idris DSL and subsequently to OpenQASM3/QIR preserve the semantics of the high-level IR.
- Idris2 DSL (low-level IR) is equipped with correctness guarantees ensuring that qubit indices are valid, control qubits are distinct from target qubits, and all control/target qubits are pairwise distinct when multiple qubits are involved.