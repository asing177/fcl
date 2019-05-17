local int a = 1;

transition initial -> terminal;

@initial
init() {
  z = sha256(a);
  terminate();
}
