global map<int, asset<int>> assMap = ( 3 : a'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65' );

transition initial -> terminal;

@initial
swapAssets(decimal<3> amount) {
  transferHoldings(sender(), lookup(3, assMap), amount, sender());
  terminate();
}
