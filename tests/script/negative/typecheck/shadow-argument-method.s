transition initial -> terminal;

@initial
init(int a) {
  a = a + 1;
  terminate();
}
