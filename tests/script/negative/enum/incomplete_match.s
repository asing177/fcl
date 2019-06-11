type T = A | B | C;

@initial
foo(T it) {
  x = case(it) {
        A -> 1;
        B -> 2
      };
  terminate();
}


