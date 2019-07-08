type T {
  C1(int f1, bool f2, text f3);
  C2(int f1, bool f2);
  C3(int f1);
}

global bool x;

@initial
go(T t) {
  x = t.f2;
  terminate()
}
