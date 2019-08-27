type bit {
  O; // zero
  I; // one
}

global bit b;

@initial
go(bit bit) {
  b = flip(bit);
  terminate();
}

flip (bit bit) {
  case(bit) {
    O -> flop(I);
    I -> O;
  };
}

flop (bit bit) {
  case(bit) {
    O -> I;
    I -> flip(I);
  };
}
