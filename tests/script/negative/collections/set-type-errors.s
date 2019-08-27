type investor {
  BigInvestor;
  MedInvestor;
  SmallInvestor;
}

global map<investor,set<account>> investors =
  ( BigInvestor : {}
  , MedInvestor : {}
  , SmallInvestor : {}
  );

@initial
insertInvestor(account a, investor x) {
  currSet = lookup(x, investors);
  newSet =
    if (!element(a, currSet)) {
      setInsert(5.48, currSet);
    } else {
      (a);
    };
  investors = mapInsert(x, newSet, investors);
  stay();
}

@initial
deleteInvestor(asset<decimal<2>> a, investor x) {
  currSet = lookup(x, investors);
  newSet =
    if (element(a, currSet)) {
      setDelete(a, currSet);
    } else {
      currSet;
    };
  mapInsert(x, newSet, investors);
  stay();
}

@initial
end() { terminate(); }
