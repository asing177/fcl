type investor {
  BigInvestor;
  MedInvestor;
  SmallInvestor;
}

global map<investor, set<account>> investors =
  ( BigInvestor : {}
  , MedInvestor : {}
  , SmallInvestor : {}
  );

transition initial -> terminal;
transition initial -> initial;

@initial
insertInvestor(account a, investor x) {
  currSet = lookup(x, investors);
  newSet =
    if (!element(a, currSet)) {
      setInsert(a, currSet);
    } else {
      {a};
    };
  investors = mapInsert(x, newSet, investors);
  stay();
}

@initial
deleteInvestor(account a, investor x) {
  currSet = lookup(x, investors);
  newSet =
    if (element(a, currSet)) {
      setDelete(a, currSet);
    } else {
      currSet;
    };
  investors = mapInsert(x, newSet, investors);
  stay();
}

@initial
end() { terminate(); }
