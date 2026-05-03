### Introduction

We present here a short glossary of useful terms in order to avoid cluttering the documentation with definitions.

### What is quantum data
By quantum data one usually means qubits viewed as a programming abstraction. In quantum programming languages, qubits are typically modeled in one of two ways:

- Resource-oriented model — qubits are treated as mutable computational resources whose quantum state evolves when quantum gates are applied.
- State-oriented model — qubits are represented as language expressions denoting vectors or states in a Hilbert space, which can be composed horisontally and vertically.
  
 However, one should keep in mind that qubits are also a physical resource in a QPU. This means that the laws of physics apply when dealing with qubits. Two no-go theorems are often mentioned in the context of quantum computing: the [no-cloning](defining-terms.md#no-cloning-theorem) theorem and no-program theorem.

### What are quantum operations
These are first of all unitary quantum gates used to apply a unitary transformation on a single qubit or on a qubit register. Non-unitary quantum operations include: **measure**, **reset**, **discard** operations applied to qubits. Within the Leaf language, a qubit register is specified as an array of qubits without any other decoration added to it.

### No cloning theorem
Due to the no-cloning theorem in quantum mechanics an arbitrary qubit cannot be copied, meaning that there is no quantum operation that given an arbitrary state $|\psi\rangle$ produces $|\psi\rangle \otimes |\psi\rangle$. Duplicating a quantum state is possible but what usually is meant by "duplicating" means making a copy of un-entangled (pure) state; for example taking the canonical basis state: $|0\rangle$ and generating: $|0\rangle  \otimes |0\rangle$.

### Linear and affine qubits
Once allocated by the software stack a quantum bit can be used by a quantum program at most once because applying an operation to a qubit will make the physical qubit transition to a different state. One could imagine a way around this limitation by making a clone of the qubit state before applying a gate but, as discussed [above](defining-terms.md#no-cloning-theorem), this operation in un-physical. Most quantum programming languages use linear or affine types to model qubits to capture this constraint in language and also the other quantum constraint given by the fact qubits cannot in all cases be simply discarded by the programmer without affecting the computation outcome. This aspect is explained in detail in the discussion on [uncomputation](defining-terms.md#what-does-uncomputation-mean) below. A linear qubit must be used exactly once. An affine qubit must be used at most once. Leaf has the notion of borrowing qubits arguments as syntactic sugar used to replace:

```leaf
let q = f(q);
```

with:

```leaf
f(&q);
```

### What are ancilla qubits?
Ancilla qubits are often used as scratch qubits needed to implement oracles in general or boolean functions in particular as a quantum circuit. Ancilla qubits are also used in order to reduce the depth of the resulting circuit when decomposing a multi-qubit unitary in one and two-qubit physical gates. Usually ancilla qubits are discarded when the algorithm no longer needs them in order to be recycled and reused for subsequent operations since any quantum processor has a finite supply of qubits. Discarding quantum data is not trivial in quantum computing, unlike the case of classical data which the programmer can simply forget about. How and when ancilla qubits can be safely discarded is discussed [below](defining-terms.md#what-does-uncomputation-mean).

### What is an oracle?
In computer science in general and quantum computing in particular an oracle provides an answer to some problem which is treated as a black-box function. A boolean function is a function that takes one or more binary inputs and produces a boolean output. A boolean oracle is a classical function:

$f:Bool^n \to Bool^m$.

Such an oracle can be used to encode algebraic functions like sin() or cos() for example. Quantum oracles are not limited to boolean functions. In the case of the Grover algorithm, where the task is to search an unsorted database, the oracle may be used to mark a given quantum state we want to identify from a superposition of quantum states, a pattern which is named in literature a [phase oracle](defining-terms.md#what-is-a-phase-oracle) since we add a phase to the sought-after state. In the case of quantum phase estimation, an oracle is needed to perform a controlled unitary transformation U on a set of qubits whose eigenvalue we want to compute. For implementing Shor's algorithm, an oracle is needed to identify periodicity in quantum Fourier transforms. 

An oracle is a theoretical construct. However, in the context of a quantum programming language, its implementation usually involves quantum circuits. Other technologies could be used in principle for implementing an oracle, but we will not expand on such possibilities here. So, barring the comment made previously, in order to be able to run on a quantum computer a boolean oracle must be implemented as a reversible circuit. One way to do this is by implementing the oracle:

$f':Bool^{(n+m)} \to Bool^{(n+m)}$

as:

$f'(x, y) = (x, y ⊕ f(x))$

via: 

$Ο(|x\rangle \otimes |y\rangle) = |x\rangle \otimes |y \oplus f(x)\rangle$

This formulation first ensures that the size of the input register is the same as the output register which opens up the possibility that there could exist a unitary transformation Ο implementing this operation. Second it ensures that $f'(x_{1}, y) \neq f'(x_{2}, y)$ in case $x_{1} \neq x_{2}$ which means that the transformation Ο is invertible which is mandatory, otherwise it could not be implemented by a unitary operation. Moreover, Ο is unitary by construction and by defining how it acts on basis states we have specified how it acts on a general state.

### What is a phase oracle?

A phase oracle is a quantum operation that encodes the result of function only in the phase of a quantum state:

$O_f |x\rangle = (-1)^{f(x)} |x\rangle$

as opposed to a standard bit flip oracle:

$O_f |x, y\rangle = |x, y \oplus f(x)\rangle$

They are equivalent — you can convert back and forth using an ancilla qubit prepared in $|-\rangle$ state.

### What does uncomputation mean?
During a quantum program, we often need [ancilla qubits](defining-terms.md#what-are-ancilla-qubits) to perform different operations, for example in order to implement a [boolean oracle](defining-terms.md#what-is-an-oracle) as a circuit. These ancilla qubits are a finite resource and typically need to be reused several times throughout a longer quantum program. Discarding a quantum register during computation is physically equivalent to measuring it because implies that a reset operation will be executed on those qubits. This implicit measurement needed during reset may impact the other qubits used to execute the quantum program if their state is entangled with ancilla qubits. This is why dropping temporary quantum data values from the program state requires explicitly applying quantum operations that uncompute those values. Note that due to the [principle of deferred measurement](defining-terms.md#what-is-principle-of-deferred-measurement), keeping those qubits and discarding them at the end of the computation may still impact the final computation result. To be more precise, uncomputing means cleaning up temporary effects on ancilla qubits.

Uncomputation is possible in principle because quantum circuits are reversible, and consequently a computation can be run in reverse. A useful feature for a quantum programming language would be safe, optionally automatic, uncomputation which means that after performing uncomputation on a register of qubits, a temporary quantum variable held by those qubits can be discarded from a program, the same manner we can discard without consequences any classical variable. In a general quantum program, not any temporary quantum data can be uncomputed since it may be entangled with multiple registers. Uncomputation is guaranteed to be done safely if the initial evaluation of data to be uncomputed can be described classically, meaning that it neither produce a superposition state out of an input basis state, nor destroy an existing superposition state, and also if the input quantum data needed for uncomputation is still present in an unaltered state.

As a simple example of how uncomputation is used, start with one qubit and an empty ancilla qubit:

$|x\rangle \otimes |0\rangle$

Apply an oracle $O$:

$|x\rangle \otimes |f(x)\rangle$

Apply Z gate on ancilla to record the result in a quantum phase:

$(-1)^{f(x)} |x\rangle \otimes |f(x)\rangle$

Uncompute by applying $O^\dagger$:

$(-1)^{f(x)} |x\rangle \otimes |0\rangle$

### What is principle of deferred measurement?

Any measurement performed in the middle of a quantum circuit can be postponed to the end—without changing the final outcome probabilities—provided you replace classical control with coherent quantum control. In more explicit terms, the following operations:

1. Measure a qubit
2. Use the classical result (0 or 1)
3. Apply a gate depending on that result

Produces the same final probabilities as:

1. Do not measure
2. Keep the qubit in superposition
3. Replace the “if” with a controlled quantum gate
4. Measure only at the end

This means we can replace:

Classical branching → quantum-controlled operations

Early measurement → final measurement

### What are quantum conditionals?

There are two flavors of quantum conditionals depending on the type of qubit. Both can be generalized quite naturally to `match` style statements where instead of just two, multiple choices can apply.

1. Resource-oriented qubit model

   A quantum conditional with a condition on a qubit `q` means applying two quantum operations on other set of qubits depending on the state of `q` coherently, without measuring it. A simple example is shown below:

   Pseudocode:

   ```text
   if q then U else V
   ```

   Given:

   $|q\rangle = \alpha |0\rangle + \beta |1\rangle$

   Targeting:

   $|q\rangle \otimes |\psi\rangle$

   The example code produces:

   $\alpha |0\rangle \otimes V|\psi\rangle + \beta |1\rangle \otimes U|\psi\rangle$

2. State-oriented qubit model

   This quantum conditional was introduced in this [paper](https://arxiv.org/pdf/quant-ph/0409065) and expanded upon in the [paper here](https://arxiv.org/pdf/0806.2735). Following the authors, here is the pseudocode for defining how CNOT gate acts on a qubit producing a state expression:

   ```text
   qnot q = 1/sqrt(2) * (if° q then qfalse else qtrue)
   ```

   where:

   ```text
   qfalse ≡ |0⟩
   qtrue  ≡ |1⟩
   ```

   QML’s if°/quantum conditional is intended for quantum control without measurement, with orthogonality restrictions such as qfalse ⟂ qtrue. The code in the two branches can be arbitrary state expressions, not just |0⟩ and |1⟩ as long as these are orthogonal. The orthogonality condition is necessary on order for the norm of the state and implicitly the unitarity of the operation to be preserved. This may seem pretty simple at first sight, but this construction is very powerful. It can be used for example to specify a QFT transformation in code starting from the mathematical denotation of the transform without the programmer needing to know on how the QFT quantum circuit should look like:

   ```text
   qft1 x =
     qcase° x of
       | qfalse => plus
       | qtrue  => minus
   ```

   where:

   ```text
   plus  = (|0⟩ + |1⟩) / √2
   minus = (|0⟩ - |1⟩) / √2
   ```

   and:

   ```text
   qft2 (x1, x0) =
     if° x1 then
       if° x0 then
         (minus, phase(3π/2))    -- input |11⟩
       else
         (plus,  minus)          -- input |10⟩
     else
       if° x0 then
         (minus, phase(π/2))     -- input |01⟩
       else
         (plus,  plus)           -- input |00⟩
   ```
