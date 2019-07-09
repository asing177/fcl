type point { P(num x, num y) }

global point p;

@initial
go(point p1, point p2) {
  p = midpoint(p1,p2);
  terminate()
}

// take the midpoint of two points
midpoint(point p1, point p2) {
  P((p1.x + p2.x)/2, (p1.y + p2.y)/2)
}
