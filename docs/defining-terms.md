### Introduction

We present here a short glossary of useful terms in order to avoid cluttering the documentation with definitions.

### What are quantum operation?
These are first of all unitary quantum gates used to apply a unitary transformation on a single qubit or on a qubit register. Non-unitary quantum operations include: **measure**, **reset**, **discard** operations applied to qubits. Within the language, a qubit register is specified as an array of qubits without any other decoration added to it.

### What are ancilla qubits?
Ancilla qubits are often used as scratch qubits needed to implement oracles in general or boolean functions in particular as a quantum circuit. Ancilla qubits are also used in order to reduce the depth of the resulting circuit when decomposing a multiqubit unitary in one and two-qubit physical gates. Usually ancilla qubits are discarded when the algorithm no longer needs them in order to be recycled and reused for subsequent operations since any quantum processor has a finite supply of qubits. Discarding quantum data is not trivial in quantum computing, unlike the case of classical data which the programmer can simply forget about. How and when ancilla qubits can be safely discarded is discussed [below](defining-terms.md#what-does-uncomputation-mean).

### What is an oracle?
In computer science in general and quantum computing in particular an oracle provides an answer to some problem which is treated as a black-box function. A boolean function is a function that takes one or more binary inputs and produces a boolean output. A boolean oracle is a classical function: f:Bool\*\*n -> Bool\*\*m. Such an oracle can be used to encode algebraic functions like sin or cos for example. Quantum oracles are not limited to boolean functions. In the case of the Grover algorithm, where the task is to search an unsorted database, the oracle may be used to mark a given quantum state we want to identify in a superposition of quantum states, a pattern which is named in literature a phase oracle since we add a phase to the sought-after state. In the case of quantum phase estimation, an oracle is needed to perform a controlled unitary transformation U on a set of qubits whose eigenvalue we want to compute. For implementing Shor's algorithm, an oracle is needed to identify periodicity in quantum Fourier transforms. An oracle is a theoretical construct. However, in the context of a quantum programming language, its implementation usually involves quantum circuits. Other technologies could be used in principle for implementing an oracle but we will not expand on such possibilities here. So, barring the comment made previously, in order to be able to run on a quantum computer a boolean oracle must be implemented as a reversible circuit. One way to do this is by implementing the oracle f':Bool\*\*(n+m) -> Bool\*\*(n+m) as:

f'(x, y) = (x, y ⊕ f(x))

via: 

Ο(|x⟩ ⊗ |y⟩) = |x⟩ ⊗ |y ⊕ f(x)⟩

This formulation first ensures that the size of the input register is the same as the output register which opens up the possibility that there could exist a unitary transformation Ο implementing this operation. Second it ensures that f'(x1,y) != f'(x2,y) in case x1 != x2 which means that the transformation Ο is invertible which is mandatory, otherwise it could not be implemented by a unitary operation. Moreover, Ο is unitary by construction and by defining how it acts on basis states we have specified how it acts on a general state.

### What does uncomputation mean?
During a quantum program, we often need [ancilla qubits](defining-terms.md#what-are-ancilla-qubits) to perform different operations, for example in order to implement a [boolean oracle](defining-terms.md#what-is-an-oracle). These ancilla qubits typically need to be reused several times throughout a longer quantum program. Discarding a quantum register during computation is physically equivalent to measuring it because implies that a reset operation will be executed on those qubits. This implicit measurement needed during reset may impact the other qubits used to execute the quantum program if their state is entangled with ancilla qubits. This is why dropping temporary quantum data values from the program state requires explicitly applying quantum operations that un-compute those values (note that due to the principle of deferred measurement, keeping those qubits and discarding them at the end of the computation may still impact the final computation result). To be more precise, uncomputing means cleaning up temporary effects on ancilla qubits. Uncomputation is possible in principle because quantum circuits are reversible and consequently a computation can be run in reverse. A useful feature for a quantum programming language would be safe (possibly automatic) uncomputation which means that a temporary quantum variable can be discarded from a program like we can discard without consequences any classical variable. In a general quantum program, not any temporary quantum data can be uncomputed since it may be entangled with multiple registers. Uncomputation is guaranteed to be done safely if the initial evaluation of data to be uncomputed can be described classically, meaning that neither does not produce a superposition state out of an input basis state, nor does not destroy a superposition state, and also if the input quantum data needed for uncomputation is still present in an unaltered state.

### What are quantum conditionals?

A quantum conditional on qubit q means applying different quantum operations depending on the state of q, coherently and without measuring it. A simple example is shown below:

pseudo-code: if q then U else V

given: ∣q⟩ = α∣0⟩ + β∣1⟩

targeting: ∣q⟩ ⊗ ∣ψ⟩

produces: α∣0⟩ ⊗ (V∣ψ⟩) + β∣1⟩ ⊗ (U∣ψ⟩)

