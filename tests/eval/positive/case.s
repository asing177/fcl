type T { A; B }
type S { C(int f1, bool f2); D(T f3) }

int i;
bool b;
T t;

@initial
f(S s) {
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
