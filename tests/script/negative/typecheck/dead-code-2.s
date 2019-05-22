// Example of an if-statement in which one branch transitions but the
// other does not.

transition initial -> terminal;
transition initial -> initial;

@initial
f () {
  if (True) {
    terminate();
  } else {
    a = 5;
  };
  stay();
}
