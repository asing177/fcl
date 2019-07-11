global int undef;
global map<int,int> theMap = (1 : undef);
global int x;

transition initial -> terminal;

@initial
go() {
  x = lookup(1,theMap);
  transitionTo(@terminal)
}
