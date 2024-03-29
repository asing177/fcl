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
  x1 = addFee(x);
  // See that we are rounding here
  x1 + round(2, x1 * rate);
}

calcAndInsert(map<account, decimal<2>> n, account b, decimal<2> v) {
  mapInsert(b, calcTotal(v), n);
}
