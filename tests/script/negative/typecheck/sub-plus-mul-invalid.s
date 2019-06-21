global int x = 0;

transition initial -> terminal;

@initial
f() {
  x = contractValue(c'1234', "foo") * false;
  x = contractValue(c'1234', "foo") + false;
  x = contractValue(c'1234', "foo") - false;
  x = contractValue(c'1234', "foo") / false;
  terminate();
}
