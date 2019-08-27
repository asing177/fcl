type direction { Left; Right }

@initial
start() {
  transitionTo(:a);
}

@a
leftOrRight(direction dir) {
  case(dir) {
    Left -> transitionTo(:b);
    Right -> transitionTo(:c);
  };
}

@b
fromBtoD() {
  transitionTo(:d);
}

@c
fromCtoD(direction dir) {
  case (dir) {
    Left -> transitionTo(:d);
    Right -> transitionTo(:d);
  };
}

@d
stop() {
  terminate();
}
