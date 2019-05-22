global map<int, text> myWrongMap = ( 3 : "wrong" );

transition initial -> terminal;

@initial
foo(int amount) {
  transferHoldings(sender(), lookup(3, myWrongMap), amount, sender());
  terminate();
}
