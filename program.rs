fn main() {
  let q0: Qubit = qalloc();
  let q1: Qubit = qalloc();

  ctrl(q0) H(q1);
  CX(q0, q1);

  let (b0, q0) = measr(q0);
  if b0 { X(q0); }

  q0
}
