// Ensure that sequencing in `case` scrutinee expressions doesn't parse
// since this assumption is made in the type checker, e.g. we don't take
// scoping into account because we can't assign any new temporary variables

enum myEnum
  { Hello
  , There
  };

@initial
run() {
  foo = case(tmp = `Hello; tmp) {
      `Hello -> 1;
      `There -> 2;
    };
  terminate();
}
