type Point = P(int x, int y);

int x;
int y;

@initial
foo(Point p) {
  x = case p {P(x1, _) -> x1};
  y = case p {P(_, y1) -> y1};
  terminate()
}