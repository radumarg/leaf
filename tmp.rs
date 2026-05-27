
//////////////////////////////
// (A) adjoint syntax
/////////////////////////////

// (1)

let f_adjoint = adjoint(f);

// (2)

adjoint {
    f(&q1, &q2, &q3);
}

// (3)

let (q1, q2, q3) = adjoint(f)(q1, q2, q3);

// or equivalently:

adjoint(f)(&q1, &q2, &q3);

// or equivalently:

adjoint {
    f(&q1, &q2, &q3);
}

// (4)

adjoint {
    H(&q1);
    CX(&q1, &q2)
}

// or equivalently:

adjoint(CX)(&q1, &q2);
adjoint(H)(&q1);

///////////////////////////////
// (B) controlled gates syntax
//////////////////////////////

// (1)

let (q0, q1, q2) = ctrl(q0, q1).apply(H)(q2);

// or equivalently:

ctrl(&q0, &q1).apply(H)(&q2);

// or equivalently:

ctrl(&q0, &q1) {
  H(&q2);
}

// (2)

let (q0, q1, q2) = ctrl(q0, q1).on(bs"10").apply(H)(q2);

// or equivalently:

ctrl(&q0, &q1).on(bs"10").apply(H)(&q2);

// or equivalently:

ctrl(&q0, &q1).on(bs"10") {
  H(&q2);
}

// (3)

let (q0, q1, q2, q3) = ctrl(q0, q1).apply(f)(q2, q3);

// or equivalently:

ctrl(&q0, &q1).apply(f)(&q2, &q3);

// or equivalently:

ctrl(&q0, &q1) {
  f(&q2, &q3);
}

// (4)

let (q0, q1, q2, q3) = ctrl(q0, q1).on(bs"10").apply(f)(q2, q3);

// or equivalently:

ctrl(&q0, &q1).on(bs"10").apply(f)(&q2, &q3);

// or equivalently:

ctrl(&q0, &q1).on(bs"10") {
  f(&q2, &q3);
}
