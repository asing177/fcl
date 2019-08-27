type t { A; B; C }

@initial
foo(t it) {
  x = case(it) {
        A -> 1;
        B -> 2
      }; // missing a case for C, will fail at runtime
  terminate();
}
