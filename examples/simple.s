/* Global variables.
 */
global int x;
global int y;

/* A method callable in the initial state. It will cause a transition from the
 * `initial` to the `xSet` state via the `transitionTo` statement.
 */
@initial
setX (int z) {
  x = z + 42;
  transitionTo(@xSet);
}

/* A method callable in the xSet state. It will cause a transition from the
 * `xSet` to the `terminal` state via the `terminate` statement.
 */
@xSet
end () {
  y = x * 2; // the checker ensures we only use initialised variables
  terminate();
}
