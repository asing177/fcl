global int x = 0;

transition initial -> terminal;

@initial
f() {
  x = contractValue(c'1234', "foo") * False;
  x = contractValue(c'1234', "foo") + False;
  x = contractValue(c'1234', "foo") - False;
  x = contractValue(c'1234', "foo") / False;
  terminate();
}
