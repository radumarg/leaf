<table>
  <tr>
    <td td align="center">
      <img src="files/leaf.png" alt="Leaf" height="200">
    </td>
    <td style="vertical-align: middle;">
      <h2 style="margin: 0;">
        Write in Leaf, prove with Lean.<br/>
        Quantum Programming 🍃⚡ 
      </h2>
    </td>
  </tr>
</table>


![Status](https://img.shields.io/badge/status-WIP-orange)
[![Tests](https://github.com/radumarg/leaf-qpl/actions/workflows/test.yml/badge.svg)](https://github.com/radumarg/leaf-qpl/actions/workflows/test.yml)
[![Issues](https://img.shields.io/github/issues/radumarg/leaf-qpl)](https://github.com/radumarg/leaf-qpl/issues)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](https://github.com/radumarg/leaf-qpl/blob/main/LICENSE)
[![Idris 2](https://img.shields.io/badge/Idris%202-v0.8.0-blue.svg)](https://github.com/idris-lang/Idris2/releases/tag/v0.8.0)
![Lean4](https://img.shields.io/badge/Lean4-theorem%20proving-6f42c1)
<!-- [![Supported By Unitary Fund](https://img.shields.io/badge/Supported%20By-UNITARY%20FUND-brightgreen.svg)](https://unitary.foundation/) -->
<!-- ![Development status: Alpha](https://img.shields.io/badge/development%20status-alpha-orange) -->

## About

* A statically typed quantum programming language with Rust-like syntax and conservative quantum extensions that preserve the look and feel of Rust.
* Linear qubit ownership and mutable borrowing: no-cloning and qubit-use discipline are enforced statically by the type checker.
* Safe ancilla management, with support for automatic uncomputation.
* Lightweight quantum contracts (`requires`, `ensures`) for expressing and checking entanglement, separation, and state-hygiene properties.
* Designed for formal verification, with program properties proved in Lean 4 against a type-safe intermediate representation.
* Detailed diagnostics designed for both human developers and AI-assisted code generation.
* Practical by construction: compiles to OpenQASM 3, with QIR support planned.
* Built for the fault-tolerant quantum computing era.

Simple code example:

```leaf
general fn coin_flip() -> bit {
    let q = qalloc();
    H(&q);
    measr(q)
}

general fn main() -> bit {
    coin_flip()
}
```

## Docs

[Documentation](docs/README.md)

## Progress Status

WIP

## Todo

[Pending Features](docs/todo.md)

