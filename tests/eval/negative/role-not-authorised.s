account bob = testAddr2;

@initial [roles: {bob, testAddr3}]
init() {
  terminate();
}
