// This script is currently causing a panic
global decimal<10> e = 2.71828182845904;
global decimal<3> x;

@initial
go(int y) {
  x = e^y;
  terminate();
}
