transition initial -> a;
transition a -> b;
transition b -> c;
transition c -> d;
transition d -> a; // only report `d` as counreachable and specifically not `a`

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

@d
go5() {
  transitionTo(@a)
}
