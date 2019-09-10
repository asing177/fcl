global text loan_contract;
global account lender;
global account borrower;
global asset<decimal<2>> currency;
global decimal<2> interest_rate;
global decimal<2> principle;

@signed [roles: lender]
loan_start() {
  transferHoldings(lender, currency, principle, borrower);
  transitionTo(@contract_active);
}

@contract_active
pay_interest() {
  interest_payment = round(2, (principle * interest_rate));
  transferHoldings(borrower, currency, interest_payment, lender);
  transitionTo(@contract_active);
}

@contract_active [roles: borrower]
payback() {
  transferHoldings(lender, currency, principle, borrower);
  transitionTo(@terminal);
}

@initial
propose_contract(decimal<2> principle_arg, asset<decimal<2>> currency_arg, account borrower_arg, account lender_arg, decimal<2> interest_rate_arg) {
  borrower = borrower_arg;
  lender = lender_arg;
  principle = principle_arg;
  currency = currency_arg;
  interest_rate = interest_rate_arg;
  transitionTo(@negotiate_terms);
}

@negotiate_terms [roles: lender]
propose_terms(text loan_contract_arg) {
  loan_contract = loan_contract_arg;
  transitionTo(@make_decision);
}

@make_decision [roles: borrower]
reject() {
  transitionTo(@terminal);
}

@make_decision [roles: borrower]
revise() {
  transitionTo(@negotiate_terms);
}

@make_decision [roles: borrower]
sign() {
  transitionTo(@signed);
}
