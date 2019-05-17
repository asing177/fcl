/*
This test file crashed caused a panic in the undeclared variable warning
generation, because a loop in the stack trace means that it does not finish
in `PlaceEnd`.
*/

global account alice;
global account bob;
global int valueAlice;
global int valueBob;
global int total;
global int proposedNewTotal;
global bool agreesAlice;
global bool agreesBob;

transition initial -> {todoAlice, todoBob};
transition todoAlice -> doneAlice;
transition todoBob -> doneBob;
transition {doneAlice, doneBob} -> totalCalculated;
transition totalCalculated -> terminal;
transition totalCalculated -> {pendingDecisionAlice, pendingDecisionBob};
transition pendingDecisionAlice -> decidedAlice;
transition pendingDecisionBob -> decidedBob;
transition {decidedAlice, decidedBob} -> totalCalculated;


@initial
init(account a, account b) {
  alice = a;
  bob = b;
  transitionTo(@{todoAlice, todoBob});
}

@todoAlice [roles: {alice}]
setValueAlice(int val) {
  valueAlice = val;
  transitionTo(@doneAlice);
}

@todoBob [roles: {bob}]
setValueBob(int val) {
  valueBob = val;
  transitionTo(@doneBob);
}

@{doneAlice, doneBob}
calculateTotal() {
  total = valueAlice + valueBob;
  transitionTo(@totalCalculated);
}

@totalCalculated
end() {
  terminate();
}

@totalCalculated
proposeNewTotal(int newTotal) {
  proposedNewTotal = newTotal;
  transitionTo(@{pendingDecisionAlice, pendingDecisionBob});
}

@pendingDecisionAlice [roles: {alice}]
agreeAlice(bool agrees) {
  agreesAlice = agrees;
  transitionTo(@decidedAlice);
}

@pendingDecisionBob [roles: {bob}]
agreeBob(bool agrees) {
  agreesBob = agrees;
  transitionTo(@decidedBob);
}

@{decidedAlice, decidedBob}
setNewTotalIfBothAgree() {
  if (agreesAlice && agreesBob) {
    total = proposedNewTotal;
  };
  transitionTo(@totalCalculated);
}
