/* This contract never reaches the terminal state. */

transition initial -> initial;
transition initial -> terminal;

@initial
init() { stay(); }
