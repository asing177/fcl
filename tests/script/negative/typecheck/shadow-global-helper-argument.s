global bool f = true;

@initial
go() {
  foo(f);
  terminate();
}

foo(bool f) {
  !f;
}
