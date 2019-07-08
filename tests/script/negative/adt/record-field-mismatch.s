type T {
  C1(int  f1, bool f2, text f3);
  C2(bool f1, text f2);
  C3(text f1);
}

global bool x;

@initial
go(T t) {
  x = t.f2;
  terminate()
}
