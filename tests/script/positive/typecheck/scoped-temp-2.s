transition initial -> terminal;

@initial
init() {
  terminate();
}

foo() { y = 1; y } // ok

bar() { y = false; y } // ok
