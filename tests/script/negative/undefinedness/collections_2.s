int undef;

transition initial -> terminal;

@initial
m() {
  foo = {undef};
  transitionTo(@terminal)
}
