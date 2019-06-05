type Size { Big; Small }

type Item {
  Chips(Size size);
  Milkshake(int millilitres, bool sprinkles);
  Salad;
}

Bogus1 bog1 = Nope; // DONE: undefined constructor check
Bogus2 bog2;        // TODO: undefined type check

@initial
foo(int volume) {
  z = Milkshake(volume, False);  // DONE: typecheck constructor expressions
  y = Milkshake(volume, volume); // DONE: typecheck constructor expressions
  x = Milkshake(1, True, 1);     // DONE: constructor arity check
  w = Chips;                     // DONE: constructor arity check
  terminate()
}

@initial
bar(Item it, Bogus b) {           // DONE: 'Bogus' is not a valid type
  x = case it {
    Milkshake(p) -> p;            // DONE: not enough arguments
    Milkshake(p1,p2) -> p2;       // DONE: pattern ok, but body is of type bool
    Milkshake(p1,p1) -> p1;       // DONE: shadowing
    Milkshake(p1,p2,p3,p4) -> p1; // DONE: too many patterns
    Milkshake(1, 2) -> 42;        // DONE: literal of wrong type
    Chips(Big) -> 0;              // DONE: ok
    Salad() -> 0;                 // DONE: ok
    Salad -> 0;                   // DONE: ok, currently no reachability check
    Chips(Little) -> 1;           // DONE: undefined constructor
    _ -> 0                        // DONE: ok, catch-all
  };
  y = case x {                    // DONE: case on arbitrary expressions
    0 -> "hello";                 // DONE: ok
    _ -> "world"                  // DONE: ok
  };
  terminate()
}