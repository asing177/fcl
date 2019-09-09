global sig signature;

transition initial -> a;
transition a -> terminal;

@initial [role: deployer()]
put(text x) {
  signature = sign(x);
  transitionTo(@a);
}

@a
end() {
  terminate();
}
