type bit { O; I }

global bit someBit;
@initial
go(bit b) {
  someBit = zero(b);
  terminate()
}

flip(bit b) {
  case(b) {
    O -> I;
    I -> O;
  };
}

zero (bit b) {
  case(b) {
    O -> O;
    I -> flip(b);
  };
}
