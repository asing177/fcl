global account myAccount = sender();

transition initial -> terminal;

@initial
go() {
  terminate();
}
