
global map<account, int> balances = ();
global bool aElemOf;
global bool notElemOf;

transition initial -> terminal;

@initial
elementTest(asset<bool> a) {
  aElemOf = element(a, balances);
  notElemOf = !element(7.89, balances);
  terminate();
}
