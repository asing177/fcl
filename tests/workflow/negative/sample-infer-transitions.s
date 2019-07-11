global num x = 0.0;
global decimal<3> f = 1.234;
global decimal<2> q;
global int y = 7;
global num v;
global asset<decimal<5>> z = a'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';
global contract c = c'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';
global account a = u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';

global datetime dt;

// Infer transitions and raise error that `@circulate` is unreachable

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

@setX [roles: { u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65' }]
setX (int j, num k) {
  x = k;
  y = y * j;
  f = 2.516 + f;
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
  q = round(2,m + 1.00 + x);
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
g (asset<int> ad, account t) {
  if (assetExists(ad) && accountExists(t)) {
    transferTo(ad, 20);
    transferFrom(ad, 20, t);
    terminate();
  } else {
    terminate();
  };
}

@initial
circulate(asset<decimal<2>> af2, decimal<2> amount) {
  circulate(af2,amount);
  // transitionTo(:circulated);
  stay()
}

// unreachable
@circulated
transfer(asset<bool> ab, account from, account to, bool amount) {
  transferHoldings(from,ab,amount,to);
  terminate();
}

add50(int xyz) { xyz + 50; }
