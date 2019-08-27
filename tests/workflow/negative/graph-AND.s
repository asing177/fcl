global int x;
global int y;

@initial
setBoth() {
  x = 10;
  transitionTo(@{xIsSet, yIsSet});
}

@yIsSet
setX2() {
  x = 10;
  transitionTo(@eitherSet);
}

@xIsSet
setY2() {
  y = 20;
  transitionTo(@eitherSet);
}

@eitherSet
setZ () {
  z = x+y;
  terminate();
}
