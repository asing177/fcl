type Direction { Left; Right }

@initial
start() {
  transitionTo(:a);
}

@a
leftOrRight(Direction dir) {
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
fromCtoD(Direction dir) {
  case (dir) {
    Left -> transitionTo(:d);
    Right -> transitionTo(:d);
  };
}

@d
stop() {
  terminate();
}
