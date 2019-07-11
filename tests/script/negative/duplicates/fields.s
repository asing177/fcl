// not ok
type t1 { C(int f, int f) }

// ok
type t2 {
  D(int g);
  E(int g);
}
