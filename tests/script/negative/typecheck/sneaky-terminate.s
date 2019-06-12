// Example of a transition happening in the body of an if-statement.

transition initial -> terminal;

@initial
f () {
  if (true) {
    terminate();
    a = 1;
  };
  terminate();
}
