type Size {
  Big;
  Small;
}

type Item {
  Chips(Size size);
  Milkshake(int millilitres, bool sprinkles);
  Salad;
}

global Bogus1 bog1 = Nope; // undefined constructor + undefined type
global Bogus2 bog2;        // undefined type

@initial
foo(int volume) {
  z = Milkshake(volume, false);
  y = Milkshake(volume, volume); // bad arg
  x = Milkshake(1, true, 1);     // bad arity
  w = Chips;                     // bad arity
  terminate()
}

@initial
bar(Item it, Bogus b) {           // undefined type
  x = case it {
    Milkshake(p) -> p;            // bad arity
    Milkshake(p1,p2) -> p2;       // branch body of wrong type
    Milkshake(p1,p1) -> p1;       // shadowing / duplicate variable patterns
    Milkshake(p1,p2,p3,p4) -> p1; // bad arity
    Milkshake(1, 2) -> 42;        // literal pattern of wrong type
    Chips(Big) -> 0;
    Salad() -> 0;
    Salad -> 0;                   // currently no reachability check
    Chips(Little) -> 1;           // undefined constructor
    _ -> 0
  };
  y = case x {
    0 -> "hello";
    _ -> "world"
  };
  terminate()
}
