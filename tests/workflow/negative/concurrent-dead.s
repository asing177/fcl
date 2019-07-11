global account alice;
global account bob;
global int valueAlice;
global int valueBob;
global int total;

@initial
requestValues(account a, account b) {
  alice = a;
  bob = b;
  transitionTo(@{todoAlice, todoBob});
}

@todoAlice [role: alice]
setValueAlice(int val) {
  valueAlice = val;
  transitionTo(@doneAlice);
}

@todoBob [role: bob]
setValueBob(int val) {
  valueBob = val;
  transitionTo(@doneBob);
}

@todoBob
bad() {
  transitionTo(@dead)
}

@{doneAlice, doneBob}
calculateTotal() {
  total = valueAlice + valueBob;
  terminate();
}
