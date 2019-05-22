transition initial -> a;
transition a -> {a, a}; // should fail duplication check
transition a -> end;
