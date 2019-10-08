global decimal<14> e = 2.71828182845904;
global num a;
global int b;
global num c;

@initial
go(int x) {
  a = e^x;
  b = 42^x;
  c = e^a;
  terminate();
}
