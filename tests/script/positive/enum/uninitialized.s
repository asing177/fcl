type Direction = Left | Right;

global int i;

@initial
start(Direction dir) {
  case(dir) {
    Left -> { i = 10; transitionTo(:assigned); };
    Right -> transitionTo(:unassigned);
  };
}

@assigned
increase() {
  i = i + 1;
  transitionTo(:assigned);
}

@assigned
assignedStop() {
  terminate();
}

@unassigned
assign(Direction dir) {
  case(dir) {
    Left -> { i = 10; transitionTo(:assigned); };
    Right -> transitionTo(:unassigned);
  };
}
