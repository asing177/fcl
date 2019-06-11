
global map<account,decimal<2>> m = ();
global num rate = 0.3;

transition initial -> terminal;

@initial
f (account a, decimal<3> v) {
  m = calcAndInsert(m, a, v);
  terminate();
}

calcAndInsert(map<account, decimal<2>> n, account b, decimal<2> v) {
  v2 = round(2,v * rate + 26.0);
  mapInsert(b, v2, n);
}
