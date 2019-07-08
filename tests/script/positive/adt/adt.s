type T { A; B }

global T t = A;

@initial
run(T val) {
  foo = case(val) {
    A -> 1;
    B -> 2;
  };
  terminate();
}
