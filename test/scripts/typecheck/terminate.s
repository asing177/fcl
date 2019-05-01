@initial
go(bool b) {
  if (b) {
    transitionTo(@terminal)
  };
}

@initial
go2(bool b) {
  if (b) {
    0
  } else {
    transitionTo(@terminal)
  }
}
