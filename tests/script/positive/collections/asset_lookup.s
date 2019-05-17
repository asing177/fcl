global map<int, asset<int>> myMap = ( 3 : a'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65' );

transition initial -> terminal;

@initial
foo(int amount) {
  transferHoldings(sender(), lookup(3, myMap), amount, sender());
  terminate();
}
