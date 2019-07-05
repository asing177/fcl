/* ----------------------------- CURRENCY SWAP ---------------------------------

Alice proposes a USD to GBP swap and transfers USD to the workflow, which keeps
these funds in escrow. Bob can accept, causing the swap to happen atomically;
Bob can reject, causing Alice's USD funds to be returned to her; Alice can
retract the offer, also causing return of her funds.

------------------------------------------------------------------------------*/

global account alice = u'2vJ8JYN43hKwZx6p3r7fuRToZ3NiqGrPRngdq63hAskf';
global account bob = u'6fgCTVBrVLiXxCdZZWBznabxwEebfu6iAP93S1s358Ca';
global asset<int> usd = a'2vJ8JYN43hKwZx6p3r7fuRToZ3NiqGrPRngdq63hAskf';
global asset<int> gbp = a'6fgCTVBrVLiXxCdZZWBznabxwEebfu6iAP93S1s358Ca';
global int usdAmount;
global int gbpAmount;

transition initial -> proposed;
transition initial -> terminal;
transition proposed -> initial;
transition proposed -> terminal;

@initial
propose(int usdProposal, int gbpProposal) {
  usdAmount = usdProposal;
  gbpAmount = gbpProposal;
  transferTo(usd, usdAmount);
  transitionTo(@proposed);
}

@initial
noDeal() {
  transitionTo(@terminal);
}

@proposed
retract() {
  transferFrom(usd, usdAmount, alice);
  transitionTo(@initial);
}

@proposed
accept() {
  transferHoldings(bob, gbp, gbpAmount, alice);
  transferFrom(usd, usdAmount, bob);
  transitionTo(@terminal);
}

@proposed
reject() {
  transferFrom(usd, usdAmount, alice);
  transitionTo(@initial);
}
