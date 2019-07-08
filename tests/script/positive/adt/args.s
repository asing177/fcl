type T = A | B(num x) | C(text y);

global T t = A;

@initial
run(T val) {
  foo = case(val) {
    A -> 1;
    B(3) -> 2;
    C("foo") -> 3;
  };

  terminate();
}
