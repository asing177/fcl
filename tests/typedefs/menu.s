type Size { Big; Small }

type Item {
  Chips(Size size);
  Milkshake(int millilitres, bool sprinkles);
  Salad;
}

// @initial
// foo(int volume) {
//   z = Milkshake(volume, False); // TODO: typecheck constructor expressions
//   terminate()
// }

@initial
bar(Item it, Bogus b) {           // TODO: 'Bogus' is not a valid type
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