
enum investor
  { BigInvestor
  , SmallInvestor
  };

global map<account, int> shares = ();
global map<account, int> runoff = ();

transition initial -> terminal;
transition initial -> initial;

@initial
insertInvestor (account a, enum investor x)  {
  shares = case(x) {
    `BigInvestor -> mapInsert(a, 100, shares);
    `SmallInvestor -> mapInsert(a, 10, shares);
  };
  stay();
}

@initial
deleteInvestor (account a) {
  shares = mapDelete(a, shares);
  runoff = mapInsert(a, 10, runoff);
  stay();
}

@initial
lookupInvestor (account a) {
  v = lookup(a,shares);
  runoff = mapInsert(a, v + 10, shares);
  stay();
}

@initial
rem100Shares (account a) {
  if (sender() == a) {
    shares = modify(a, subtract100, shares);
    runoff = modify(a, add100, runoff);
  };
  stay();
}

@initial
end () { transitionTo(@terminal) }

subtract100(int v) {
  rem = v - 100;
  if (rem >= 0) { rem } else { 0 }
}

add100(int v) { v + 100 }
