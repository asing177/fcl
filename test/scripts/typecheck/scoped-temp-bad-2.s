transition initial -> terminal;

@initial
foo () {
  if (True) {
    tmp = True;
    if (True) {
      tmp = "bla" // bad
    }
  } else {
    tmp = "bla"
  };
  transitionTo(@terminal)
}
