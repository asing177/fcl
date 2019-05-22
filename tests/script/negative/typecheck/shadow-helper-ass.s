int x;

@initial
go() {
  helper1 = helper2;
  x = helper1();
  transitionTo(@terminal)
}

helper1() { 1 }

helper2() { 2 }