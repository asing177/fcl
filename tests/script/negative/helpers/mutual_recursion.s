type Bit = O | I;

global Bit b;

@initial
go(Bit bit) {
  b = flip(bit);
  terminate();
}

flip (Bit bit) {
  case(bit) {
    O -> flop(I);
    I -> O;
  };
}

flop (Bit bit) {
  case(bit) {
    O -> I;
    I -> flip(I);
  };
}
