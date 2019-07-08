type T {
  C1(int f1, bool f2, text f3);
  C2(int f1, bool f2);
  C3(int f1);
}

global T t;
global bool x;

@initial
go() {
  x = t.f2;
  terminate()
}
