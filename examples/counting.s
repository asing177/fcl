
global set<account> members = {};
global map<account, int> counters = ();

transition initial -> counting;
transition counting -> counting;
transition counting -> terminal;

@initial
start () { transitionTo(@counting); }

@counting
increment () {
  if (element(sender(), members)) {
    counters = modify(sender(), add1, counters);
  } else {
    members = setInsert(sender(), members);
    counters = mapInsert(sender(), 0, counters);
  };
  stay();
}

@counting
reset () {
  if (element(sender(), members)) {
    counters = modify(sender(), zero, counters);
  } else {
    members = setInsert(sender(), members);
    counters = mapInsert(sender(), 0, counters);
  };
  stay();
}

@counting
leave () {
  if (element(sender(), members)) {
    members = setDelete(sender(), members);
    counters = mapDelete(sender(), counters);
  };
  stay();
}

// Not able to be terminated
@counting
end () {
  if (0 > 1) { terminate(); } else { stay(); };
}

add1 (int x) { x + 1; }
zero (int x) { 0; }
