
global map<account, num> balances =
  ( u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65' : 1.0
  , u'fwBVDsVh8SYQy98CzYpNPcbyTRczVUZ96HszhNRB8Ve'  : 2.0
  , u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc' : 3.0
  );

global set<map<account, num>> woahSetOfMaps =
  { ( u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65' : 1.0
    , u'fwBVDsVh8SYQy98CzYpNPcbyTRczVUZ96HszhNRB8Ve'  : 2.0
    )
  , ( u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc' : 3.0
    )
  };

transition initial -> terminal;

@initial
applyInterest() {
  balances = transform(calcInterest, balances);
  woahSetOfMaps = transform(calcInterestSet, woahSetOfMaps);
  transitionTo(:terminal);
}

calcInterest(num balance) {
  balance * 0.34567 + balance;
}

calcInterestSet(map<account, num> bals) {
  transform(calcInterest, bals);
}
