global map<int, assetDisc> assetDiscMap = ( 3 : a'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65' );

transition initial -> terminal;

@initial
swapAssets(fixed3 amount) {
  transferHoldings(sender(), lookup(3, assetDiscMap), amount, sender());
  terminate();
}
