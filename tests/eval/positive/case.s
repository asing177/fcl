type t { A; B }
type s { C(int f1, bool f2); D(t f3) }

global int i;
global bool b;
global t t;

@initial
f(s s) {
  case s {
    D(t1) -> t = t1;
    C(0, b1) -> b = b1;
    C(i1, false) -> i = i1;
    C(_, _) -> t = B;
  };
  stay();
}

@initial
g() { terminate() }
