type Bit { O; I }

@initial
go(Bit bit) {
  b = zero(bit);
  terminate()
}

flip(Bit bit) {
  case(bit) {
    O -> I;
    I -> O;
  };
}

zero (Bit bit) {
  case(bit) {
    O -> O;
    I -> flip(bit);
  };
}
