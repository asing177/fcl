@{lhsOut, rhsOut}
join() {
  transitionTo(@terminal);
}

@initial
split() {
  transitionTo(@{lhsIn, rhsIn});
}

@lhsIn
t1() {
  transitionTo(@lhsOut);
}

@rhsIn
t2() {
  if (true) {
    if (true) {
      transitionTo(@rhsOut);
    } else {
      transitionTo(@rhsIn);
    };
  } else {
    transitionTo(@rhsOut);
  };
}
