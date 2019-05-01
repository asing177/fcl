transition initial -> terminal;

@initial
foo () {
  if (True) {
    tmp = True
  } else {
    tmp = "bla" // ok, the other `tmp` is not in scope
  };
  transitionTo(@terminal)
}

@initial
bar() {
  after (now()) {
    tmp = True
  };
  tmp = 1; // also ok
  transitionTo(@terminal)
}
