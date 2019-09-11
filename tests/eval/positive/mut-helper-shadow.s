global int i;
global int j;

@initial
go() {
  x = 0;
  i = f(x);
  j = x;
  terminate()
}

f(int x) {
  x = x+1;
  x * 2
}
