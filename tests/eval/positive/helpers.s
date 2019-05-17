global map<account,decimal<2>> m = ();

transition initial -> terminal;

@initial
f (account a, decimal<2> v) {
  m = calcAndInsert(m, a, v);
  terminate();
}

addFee(decimal<2> x) { x + 123.45; }

calcTotal(decimal<2> x) {
  rate = 0.3;
  x' = addFee(x);
  x' + x' * rate;
}

calcAndInsert(map<account, decimal<2>> n, account b, decimal<2> v) {
  mapInsert(b, calcTotal(v), n);
}
