@initial
requestValues() {
  transitionTo(@{todoAlice1, todoBob1});
}

@initial
alternativeAlice() {
  transitionTo(@todoAlice2)
}

@todoAlice1
shorterCut() {
  transitionTo(@doneAlice)
}

@todoAlice2
shortCut() {
  transitionTo(@final)
}

@todoAlice1
setValueAlice(int val) {
  transitionTo(@doneAlice);
}

@todoBob1
setValueBob1(int val) {
  transitionTo(@todoBob2);
}

@todoBob2
setValueBob2(int val) {
  transitionTo(@doneBob);
}

@{doneAlice, doneBob}
calculateTotal() {
  transitionTo(@final);
}

@final
finish() {
  terminate()
}
