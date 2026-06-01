///////////////////////////////////////////////////////////
// Deutsch-Jozsa implementation emphasizing phase kickback
///////////////////////////////////////////////////////////

unitary fn prepare_minus(q: qubit) -> qubit {
    let q = X(q);
    let q = H(q);
    q
}

uncompsafe fn balanced_reversible_oracle(
    qs: [qubit; 4],
    target: qubit
) -> ([qubit; 4], qubit)
        requires basis(target, "X")
        ensures  basis(target, "X")
        ensures  product(target, qs) {
    ctrl(&qs[0]).on("0").apply(X)(&target);
    ctrl(&qs[1]).on("1").apply(X)(&target);
    ctrl(&qs[2]).on("0").apply(X)(&target);
    ctrl(&qs[3]).on("1").apply(X)(&target);
    (qs, target)
}

// phase kickback: turning a bit-flip oracle into a phase oracle
unitary fn phase_kickback(
    qs: [qubit; 4],
    oracle: uncompsafe fn(qs: [qubit; 4], target: qubit) -> ([qubit; 4], qubit)
        requires basis(target, "X")
        ensures basis(target, "X")
        ensures product(target, qs)
) -> [qubit; 4] {
    // scratch ancilla qubit is uncomputed and reclaimed
    // automatically at the end of its scope
    let scratch ancilla = qalloc();
    let ancilla = prepare_minus(ancilla);
    let (qs, ancilla) = oracle(qs, ancilla);
    qs
}

general fn deutsch_jozsa(
    oracle: unitary fn(qs: [qubit; 4], target: qubit) -> ([qubit; 4], qubit)
) -> [bit; 4] {
    let qs = qalloc(4);

    for q in &qs {
        H(&q);
    }

    let qs = phase_kickback(qs, oracle);

    for q in &qs {
        H(&q);
    }

    measr(qs)
}

general fn main() -> [bit; 4] {
    deutsch_jozsa(balanced_reversible_oracle)
}