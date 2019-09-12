global account counterparty;
global int proposedNewTotal;
global int total;
global int valueBob;
global int valueAlice;
global account bob;
global account alice;

@agreeAmendment [roles: counterparty]
agreeAmendment(bool agrees) {
  if (agrees) {
    total = proposedNewTotal;
  };
  transitionTo(@totalCalculated);
}

@{doneAlice, doneBob}
calculateTotal() {
  total = (valueAlice + valueBob);
  transitionTo(@totalCalculated);
}

@totalCalculated
end() {
  transitionTo(@terminal);
}

@initial
init(account a, account b) {
  alice = a;
  bob = b;
  transitionTo(@{todoAlice, todoBob});
}

@totalCalculated [roles: {alice,bob}]
proposeNewTotal(int newTotal) {
  proposedNewTotal = newTotal;
  if ((sender() == alice)) {
    counterparty = bob;
    transitionTo(@agreeAmendment);
  } else {
    if (((!(sender() == bob)) && (!(sender() == alice)))) {
      transitionTo(@totalCalculated);
    } else {
      counterparty = alice;
      transitionTo(@agreeAmendment);
    };
  };
}

@todoAlice [roles: alice]
setValueAlice(int val) {
  valueAlice = val;
  transitionTo(@doneAlice);
}

@todoBob [roles: bob]
setValueBob(int val) {
  valueBob = val;
  transitionTo(@doneBob);
}
