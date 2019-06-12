type T = A | B;

@initial
run(T arg) {
  case(arg) {
    A -> terminate();
    B -> terminate();
    A -> terminate(); // unreachable match
  }
}
