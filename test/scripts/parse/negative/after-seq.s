// Ensure that sequencing in `after` guard expressions doesn't parse
// since this assumption is made in the type checker, e.g. we don't take
// scoping into account because we can't assign any new temporary variables

@initial
foo() {
  after (tmp = "2015-10-10T00:00:00Z"; tmp) {
    transitionTo(@terminal)
  }
}
