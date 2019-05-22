// All bool values should be "true"

global map<account, int> balances = ();
global bool aElemOf;
global bool notElemOf;

global set<int> amounts = {1,2,3};
global bool elemOfSet;
global bool notElemOfSet;
global account b = u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';
transition initial -> terminal;

@initial
elementTest(account a, int amount) {
  // tests for element of map
  balances = mapInsert(a, amount, balances);
  aElemOf = element(a, balances);
  notElemOf = !element(b, balances);

  // tests for element of set
  amounts = setInsert(amount, amounts);
  elemOfSet = element(amount, amounts);
  notElemOfSet = !element(0, amounts);

  terminate();
}
