global num x = 0.0;
global decimal<3> f3 = 1.234;
global decimal<2> q;
local int y = 7;
local num v;
asset<decimal<5>> z = a'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';
contract c = c'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';
account a = u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';
datetime dt;

@initial
setDate() {
  dt = "2020-10-20T15:50:12+00:00";
  terminate();
}

@initial
initialize () {
  transitionTo(:setX);
}

@setup
confirm () {
  transitionTo(:confirmation);
}

@confirmation
settle () {
  transitionTo(:settlement);
}

@settlement
finalize () {
  transitionTo(:terminal);
}

@setX
setX (int j, num k) {
  x = k;
  y = y * j;
  f3 = 2.516 + f3;
  x = k + x;
  transitionTo(:update);
}

@setX
fixX () {
  transitionTo(:setup);
}

@update
fixY () {
  transitionTo(:setup);
}

@update
update () {
  j = 10 + 7 * 10;
  k = j;
  l = k;
  m = 1.23 + 4.56 - 7.89 * 9.87 / 65.43;
  q = 1 + round(2,x + m);
  transitionTo(:setX);
}

@initial
f (int j, bool k) {
  if (k) {
    terminate();
  } else {
    terminate();
  };
}

@initial
g (asset<int> f, account t) {
  if (assetExists(f) && accountExists(t)) {
    transferTo(f, 20);
    transferFrom(f, 20, t);
  };
  terminate();
}

@initial
circulate(asset<decimal<2>> af2, decimal<2> amount) {
  circulate(af2, amount);
  transitionTo(:circulated);
}

@circulated
transfer(asset<bool> ab, account from, account to, bool amount) {
  transferHoldings(from,ab,amount,to);
  terminate();
}