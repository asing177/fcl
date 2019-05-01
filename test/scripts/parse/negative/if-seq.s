// Ensure that sequencing in `if` guard expressions doesn't parse
// since this assumption is made in the type checker, e.g. we don't take
// scoping into account because we can't assign any new temporary variables

@initial
foo() {
  if (tmp = 1; tmp == 1) {
    transitionTo(@terminal)
  }
}
