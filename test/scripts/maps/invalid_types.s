
global map<account, int> shares = ();

transition initial -> terminal;

@initial
insertTest(account a, datetime d, fixed6 amount) {
  x = mapInsert(d, 5, shares);
  y = mapInsert(a, amount, shares);
  z = mapInsert(a, 500, shares);
  terminate();
}

@initial
deleteTest(assetFrac5 a) {
  w = mapDelete(a, shares);
  terminate();
}

@initial
lookupTest(msg m) {
  v = lookup(m,shares);
  terminate();
}
