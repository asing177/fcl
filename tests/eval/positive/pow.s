global num e = 2.71828182845904;
global decimal<3> x;

@initial
go(num y) {
  x = round(3, e^y);
  terminate();
}
