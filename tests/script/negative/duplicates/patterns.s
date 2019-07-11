type t { C(int f1, bool f2) }

global bool b;

@initial
f(t t) {
  b = case t { C(x,x) -> x };
  terminate()
}
