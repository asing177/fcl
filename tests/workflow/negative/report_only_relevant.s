transition initial -> a;
transition a -> b;
transition b -> c;
transition c -> d; // only report `d` as counreachable, not `initial`, `a`, etc.

@initial
go1() {
  transitionTo(@a)
}

@a
go2() {
  transitionTo(@b)
}

@b
go3() {
  transitionTo(@c)
}

@c
go4() {
  transitionTo(@d)
}
