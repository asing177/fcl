transition initial -> {a, b};
transition a -> c;
transition b -> d;
transition {c,d} -> terminal;

@initial
f () { transitionTo(:{a,b}); }

@a
g () { transitionTo(:c); }

@b
h () { transitionTo(:d); }

@{c,d}
i () { transitionTo(:terminal); }
