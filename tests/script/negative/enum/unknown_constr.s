type T = C;

@initial
run(T arg) {
  someInt = case(arg) {
    A -> 10;
    B -> 20;
  };
  terminate()
}
