global int x;

@initial
go(bool c1, bool c2) {
  if (c1) {
    x = 0;
  } else if (c2) {
    x = 1;
  } else {
    x = 2;
  };
  stay()
}

@initial
end() {
  terminate()
}
