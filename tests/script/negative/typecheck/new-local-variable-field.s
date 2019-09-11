@initial
go() {
  c.f = 1; // don't make `c` a new local variable
  terminate();
}
