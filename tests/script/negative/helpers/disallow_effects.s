
global bool b = false;
global bool f = true;

transition initial -> terminal;

@initial
noeffects() {
  b = readWrite(f);
  tryTransition();
  terminate();
}

// Reads/Writes should not be allowed in helpers
// 'a' read should be allowed (argument reads permitted)
readWrite(bool a) {
  !b && a;
}

// Transitions (write effects) should not be allowed in helpers
// Assignment to 'b' should be disallowed
// read/write of 'x' and 'y' should be allowed
tryTransition(int x) {
  y = x + 1;
  z = y + 2;
  b = z > 5;
  transitionTo(:terminal);
}
