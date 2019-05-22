
global map<account, int> shares = ();

transition initial -> terminal;

@initial
insertTest(account a, datetime d, decimal<6> amount) {
  mapInsert(d, 5, shares);
  mapinsert(a, amount, shares);
  mapinsert(a, 500, shares);
  terminate();
}

@initial
deleteTest(asset<decimal<5>> a) {
  mapDelete(a, shares);
  terminate();
}

@initial
lookupTest(msg m) {
  v = lookup(m,shares);
  terminate();
}
