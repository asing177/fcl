type T = C(int f1, bool f2);

bool b;

@initial
f(T t) {
  b = case t { C(x,x) -> x };
  terminate()
}