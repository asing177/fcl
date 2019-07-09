type point { P(int x, int y) }

global int x;
global int y;

@initial
foo(point p) {
  x = case p {P(x1, _) -> x1};
  y = case p {P(_, y1) -> y1};
  terminate()
}
