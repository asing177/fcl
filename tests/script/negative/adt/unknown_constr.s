type t { C }

@initial
run(t arg) {
  someInt = case(arg) {
    A -> 10;
    B -> 20;
  };
  terminate()
}
