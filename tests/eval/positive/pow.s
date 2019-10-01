global decimal<14> e = 2.71828182845904;
global num x;

@initial
go(int y) {
  x = e^y;
  terminate();
}
