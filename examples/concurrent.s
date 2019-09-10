/* ------------------------- A CONCURRENT WORKFLOW -----------------------------

Alice and Bob both need to pick a number. It doesn't matter which order this
happens in. Once both have picked their number, we calculate the total sum.

------------------------------------------------------------------------------*/

global account alice;
global account bob;
global int valueAlice;
global int valueBob;
global int total;

transition initial -> {todoAlice, todoBob};
transition todoAlice -> doneAlice;
transition todoBob -> doneBob;
transition {doneAlice, doneBob} -> terminal;

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

@{doneAlice, doneBob}
calculateTotal() {
  total = valueAlice + valueBob;
  terminate();
}
