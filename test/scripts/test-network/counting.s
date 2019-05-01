
global map<account, int> counters = ();

transition initial -> counting;
transition counting -> terminal;

@initial
start () { transitionTo(@counting); }

@counting
join () {
  mapInsert(sender(), 0, counters);
}

@counting
increment () {
  modify(sender(), add1, counters);
}

@counting
reset () {
  modify(sender(), zero, counters);
}

// Not able to be terminated
@counting
end () {
  if (0 > 1) { terminate(); };
}

add1 (int x) { x + 1; }
zero (int x) { 0; }
