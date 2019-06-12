transition initial -> terminal;

@initial
foo () {
  if (true) {
    tmp = true;
    if (true) {
      tmp = "bla" // bad
    }
  } else {
    tmp = "bla"
  };
  transitionTo(@terminal)
}
