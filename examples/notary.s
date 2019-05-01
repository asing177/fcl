global sig signature;

transition initial -> set;
transition set -> terminal;

@initial [role: deployer()]
put(text x) {
  signature = sign(x);
  transitionTo(@set);
}

@set
end() {
  terminate();
}
