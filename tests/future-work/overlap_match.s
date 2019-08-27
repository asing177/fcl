type t { A; B }

@initial
run(t arg) {
  case(arg) {
    A -> terminate();
    B -> terminate();
    A -> terminate(); // unreachable match
  }
}
