transition initial -> terminal;

@initial
foo () {
  if (true) {
    tmp = true
  } else {
    tmp = "bla" // ok, the other `tmp` is not in scope
  };
  transitionTo(@terminal)
}

@initial
bar() {
  after (now()) {
    tmp = true
  };
  tmp = 1; // also ok
  transitionTo(@terminal)
}
