@initial
init() {
  a = (b = 10); // rhs of assignment must be an expression (not 'void')
  terminate();
}
