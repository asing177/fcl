// Everything good in this test.

transition initial -> a;
transition a -> a;
transition a -> b;
transition b -> c;
transition b -> d;
transition c -> e;
transition c -> f;
transition d -> e;
transition f -> e;
transition e -> terminal;

@initial
toA() {
  transitionTo(:a);
}

@a
toB() {
  myDate = "2018-01-22T13:46:10+00:00";

  if ((myDate < now()) && (now() < myDate + 7d)) {
    transitionTo(:b);
  } else {
    stay();
  };
}

@b
toCorD() {
  if (now() < "2018-01-22T13:46:10+00:00") {
    transitionTo(:c);
  } else {
    transitionTo(:d);
  };
}

@c
toEorF() {
  if (true) {
    transitionTo(:e);
  } else {
    transitionTo(:f);
  };
}

@d
toEfromD() {
  transitionTo(:e);
}

@f
toEfromF() {
  transitionTo(:e);
}

@e
finish() {
  terminate();
}
