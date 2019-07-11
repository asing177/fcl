type t { A; B(num x); C(text y) }

global t t = A;

@initial
run(t val) {
  foo = case(val) {
    A -> 1;
    B(3) -> 2;
    C("foo") -> 3;
  };

  terminate();
}
