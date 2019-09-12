global int x;

@initial
init(int a) {
  x = abs(a);
  stay()
}

@initial
end() {
  terminate()
}

abs(int a) {
  if (a < 0) {
    !a
  } else {
    a
  }
}
