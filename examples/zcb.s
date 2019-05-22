/* ----------------------------- ZERO COST BOND --------------------------------

This example workflow models a situation where a bond issuer issues a zero cost
cost bond to an investor, usually at a steep discount compared to its face value
(the price which the issuer has to pay when buying back the bond).

------------------------------------------------------------------------------*/

/*  *  *  *  *  *  *  *  *  *  *  Variables  *  *  *  *  *  *  *  *  *  *  *  */

account bond_issuer; // The issuer of the bond
account investor; // The investor who is buying the bond

asset<decimal<2>> the_asset; // USD/GBP/EUR etc.
decimal<2> face_value; // The face value
decimal<2> issue_price; // The issue price of the bond

datetime offer_made; // The date at which the offer was made
datetime offer_expiry; // The date when the deployer can retract the offer

datetime bond_issued; // The date at which the offer was accepted
timedelta time_until_maturity; // The length of the period between issuing and maturity;
datetime maturity; // The date of maturity

/*  *  *  *  *  *  *  *  *  *  Transitions   *  *  *  *  *  *  *  *  *  *  *  */

transition initial -> offer_made;
transition initial -> terminal;
transition contract_agreed -> terminal;
transition offer_made -> initial;
transition offer_made -> contract_agreed;

/*  *  *  *  *  *  *  *  *  *  *   Methods   *  *  *  *  *  *  *  *  *  *  *  */

// Bond issuer may terminate the workflow if it is in the inital state
@initial [role: deployer()]
kill() {
  terminate();
}

// Instantiate an offer to a particular investor
@initial [role: deployer()]
make_offer( account investor_p,
            asset<decimal<2>> asset_p,
            decimal<2> face_value_p,
            decimal<2> issue_price_p,
            timedelta offer_validity_p,
            timedelta time_until_maturity_p
          ) {
  bond_issuer = deployer();
  investor = investor_p;
  the_asset = asset_p;
  face_value = face_value_p;
  issue_price = issue_price_p;
  time_until_maturity = time_until_maturity_p;
  offer_made = now();
  offer_expiry = offer_made + offer_validity_p;
  transitionTo(@offer_made);
}

// Investor declines the offer
@offer_made [role: investor]
decline_offer() {
  transitionTo(@initial);
}

// Bond issuer may retract the offer after the offer validity has expired
@offer_made [after: offer_expiry, role: bond_issuer]
retract_offer() {
  transitionTo(@initial);
}

// Investor accepts the offer and the asset is transferred in the same step,
// so the investor can only accept the offer if he has sufficient liquidity.
@offer_made [role: investor]
accept_offer() {
  transferHoldings(investor, the_asset, issue_price, bond_issuer);
  bond_issued = now(); // The clock until maturity starts ticking.
  maturity = bond_issued + time_until_maturity;
  transitionTo(@contract_agreed);
}

// The bond issuer pays out the face value after maturity
@contract_agreed [after: maturity, role: bond_issuer]
settle() {
  transferHoldings(bond_issuer, the_asset, face_value, investor);
  terminate();
}
