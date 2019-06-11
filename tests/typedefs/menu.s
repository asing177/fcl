type Size { Big; Small }

// type Size { Grand; Petit } // DONE: duplicate type definition

// type Taille { Big; Small } // DONE: constructor name shadowing check

type Dup { MkDup(int n, int n) } // DONE: field name shadowing check

type Item {
  Chips(Size size);                            // DONE: Parameterised constructors with field names
  Milkshake(int millilitres, bool sprinkles);  // ok
  Salad;                                       // ok
}

Bogus1 bog1 = Nope; // DONE: undefined constructor check
Bogus2 bog2;        // TODO: undefined type check

@initial
foo(int volume) {
  z = Milkshake(volume, false);  // DONE: typecheck constructor expressions
  y = Milkshake(volume, volume); // DONE: typecheck constructor expressions
  x = Milkshake(1, true, 1);     // DONE: constructor arity check
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