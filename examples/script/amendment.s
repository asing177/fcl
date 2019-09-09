/* ----------------------- MUTUALLY AGREED AMENDMENT ---------------------------

Alice and Bob both commit their value, then we calculate the total. Either may
then choose to propose an amended total value, which will be accepted if the
counterparty agrees.

------------------------------------------------------------------------------*/

global account alice;
global account bob;
global int valueAlice;
global int valueBob;
global int total;
global int proposedNewTotal;
global account counterparty;

transition initial -> {todoAlice, todoBob};
transition todoAlice -> doneAlice;
transition todoBob -> doneBob;
transition {doneAlice, doneBob} -> totalCalculated;
transition totalCalculated -> terminal;
transition totalCalculated -> agreeAmendment;
transition totalCalculated -> totalCalculated;
transition agreeAmendment -> totalCalculated;

@initial
init(account a, account b) {
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
  transitionTo(@totalCalculated);
}

@totalCalculated
end() {
  terminate();
}

@totalCalculated [roles: {alice, bob}]
proposeNewTotal(int newTotal) {
  proposedNewTotal = newTotal;
  if (sender() == alice) {
    counterparty = bob;
    transitionTo(@agreeAmendment);
  } else {
      if (sender() == bob) {
      counterparty = alice;
      transitionTo(@agreeAmendment);
      } else {
      stay();
      };
  };
}

@agreeAmendment [role: counterparty]
agreeAmendment(bool agrees) {
  if (agrees) {
    total = proposedNewTotal;
  };
  transitionTo(@totalCalculated);
}
