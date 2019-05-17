int undef;
map<int,int> theMap = (1 : undef);
int x;

transition initial -> terminal;

@initial
go() {
  x = lookup(1,theMap);
  transitionTo(@terminal)
}
