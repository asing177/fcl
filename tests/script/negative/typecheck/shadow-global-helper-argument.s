global bool f = True;

@initial
go() {
  foo(f);
  terminate();
}

foo(bool f) {
  !f;
}
