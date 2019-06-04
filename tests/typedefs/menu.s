

type Size { Big; Small }

type Item {
  Chips(Size size);
  Milkshake(int millilitres, bool sprinkles);
  Salad;
}

foo(int volume) {
  Milkshake //(volume, False)
}

bar(Item it) {
  case (it) {
    Milkshake(vol, sprinkles) -> vol;
    _ -> 0
  }
}