type Point = P(num x, num y);

global Point p;

@initial
go(Point p1, Point p2) {
  p = midpoint(p1,p2);
  terminate()
}

// take the midpoint of two points
midpoint(Point p1, Point p2) {
  P((p1.x + p2.x)/2, (p1.y + p2.y)/2)
}
