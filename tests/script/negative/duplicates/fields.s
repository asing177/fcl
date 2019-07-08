// not ok
type T1 { C(int f, int f) }

// ok
type T2 {
  D(int g);
  E(int g);
}
