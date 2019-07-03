global account alice;
global account bob;
global int valueAlice;
global int valueBob;
global int total;

@initial
requestValues(account a, account b) {
  alice = a;
  bob = b;
  transitionTo(@{todoAlice1, todoBob1});
}

@initial
alternativeAlice() {
  transitionTo(@todoAlice2)
}

@todoAlice1
shorterCut() {
  transitionTo(@final)
}

@todoAlice2
shortCut() {
  transitionTo(@final)
}

@todoAlice1 [role: alice]
setValueAlice(int val) {
  valueAlice = val;
  transitionTo(@doneAlice);
}

@todoBob1 [role: bob]
setValueBob1(int val) {
  valueBob = val;
  transitionTo(@todoBob2);
}

@todoBob2 [role: bob]
setValueBob2(int val) {
  valueBob = val;
  transitionTo(@doneBob);
}

@{doneAlice, doneBob}
calculateTotal() {
  total = valueAlice + valueBob;
  transitionTo(@final);
}

@final
finish() {
  terminate()
}
