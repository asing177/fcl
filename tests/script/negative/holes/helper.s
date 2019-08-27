global map<account,decimal<2>> m = ();

@initial
f (account a, decimal<2> v) {
  m = calcAndInsert(?, ?, ?);
  terminate();
}

calcTotal(decimal<2> x) {
  rate = 0.3;
  x + x * rate;
}

calcAndInsert(map<account, decimal<2>> n, account b, decimal<2> v) {
  v1 = round(2, calcTotal(v));
  mapInsert(?, ?, ?);
}
