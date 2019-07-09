type t { A; B }

global t t = A;

@initial
run(t val) {
  foo = case(val) {
    A -> 1;
    B -> 2;
  };
  terminate();
}
