text x;

@initial
foo(int m, int n) {
  x = case (m + n) {
    0 -> "none";
    1 -> "one";
    _ -> "tons"
  };
  terminate()
}