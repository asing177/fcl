local num x = 0.1;

transition initial -> a;
transition a -> terminal;

@a
end () {
  terminate();
}

@initial
setX () {
  x = x * 3.0;
  transitionTo(@a);
}
