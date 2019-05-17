
global int total = 0;
global decimal<2> total' = 0.00;

global map<account, int> balances =
  ( u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65' : 1
  , u'fwBVDsVh8SYQy98CzYpNPcbyTRczVUZ96HszhNRB8Ve'  : 2
  , u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc' : 3
  );

transition initial -> terminal;

@initial
sumBalances() {
  total = aggregate(total,sumPlusOne,balances);
  transitionTo(:terminal);
}

// @initial
// multBalances() {
//   total' = aggregate(total', productPlusOne, balances);
//   transitionTo(:terminal);
// }

sum (int x, int y) { x + y; }
sumPlusOne (num x, int y) {
  sum(5,y) + 1;
}

// productPlusOne(num x, decimal<3> y) {
//   x * y + 1.00;
// }
