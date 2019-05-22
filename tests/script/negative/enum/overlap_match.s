enum enumOne { Foo, Bar };

transition initial -> terminal;

@initial
run(enum enumOne arg) {
  case(arg) {
    `Foo -> terminate();
    `Bar -> terminate();
    `Foo -> terminate();
  };
}
