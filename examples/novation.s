/* ------------------------------- NOVATION ------------------------------------

The owner of the contract must begin this contract, during which the contract
opens up for both bob and alice to join. Once bob and alice join the contract,
the both must set a value, or do some arbitrary action concurrently. After both
alice and bob set their values, the owner can choose to terminate the
contract, or _novate_ the contract, during which the joining workflow is opened
up again, and new accounts can join as alice or bob.

------------------------------------------------------------------------------*/
account alice;
account bob;

int aliceVal;
int bobVal;

transition initial -> {aliceJoining, bobJoining};
transition aliceJoining -> aliceJoined;
transition bobJoining -> bobJoined;
transition aliceNovating -> aliceJoined;
transition bobNovating -> bobJoined;
transition {aliceJoined, bobJoined} -> {aliceDoing, bobDoing};
transition aliceDoing -> aliceDid;
transition bobDoing -> bobDid;
transition {aliceDid, bobDid} -> {aliceNovating, bobNovating};
transition {aliceDid, bobDid} -> terminal;

@initial [role: deployer()]
join() {
  transitionTo(@{aliceJoining, bobJoining});
}

@aliceJoining
joinAlice() {
  alice = sender();
  transitionTo(@aliceJoined);
}

@bobJoining
joinBob() {
  bob = sender();
  transitionTo(@bobJoined);
}

@{aliceJoined, bobJoined} [roles: { alice, bob } ]
begin() {
  transitionTo(@{aliceDoing, bobDoing});
}

@aliceDoing [role: alice]
doAlice(int val) {
  aliceVal = val;
  transitionTo(@aliceDid);
}

@bobDoing [role: bob]
doBob(int val) {
  bobVal = val;
  transitionTo(@bobDid);
}

@{aliceDid, bobDid} [role: deployer()]
novate() {
  transitionTo(@{aliceNovating, bobNovating});
}

@aliceNovating
novateAlice() {
  alice = sender();
  transitionTo(@aliceJoined);
}

@bobNovating
novateBob() {
  bob = sender();
  transitionTo(@bobJoined);
}

@{aliceDid, bobDid} [roles: { alice, bob }]
end() {
  terminate();
}
