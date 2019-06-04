decimal<2> principle;
decimal<2> interest_rate;
asset<decimal<2>> currency;
account borrower;
account lender;
text loan_contract;

@initial
propose_contract(decimal<2> principle_arg, asset<decimal<2>> currency_arg, account borrower_arg, account lender_arg, decimal<2> interest_rate_arg){
  borrower = borrower_arg;
  lender = lender_arg;
  principle = principle_arg;
  currency = currency_arg;
  interest_rate = interest_rate_arg;
  transitionTo(@negotiate_terms)
}

// The lender gives their terms and conditions
@negotiate_terms [role: lender]
propose_terms(text loan_contract_arg){
  loan_contract = loan_contract_arg;
  transitionTo(@make_decision);
}

// The borrower accepts and signs
@make_decision [role: borrower]
sign(){ transitionTo(@signed) }

// The borrower requests an amendment to the terms and conditions
@make_decision [role: borrower]
revise(){ transitionTo(@negotiate_terms) }

// The borrower rejects the offer
@make_decision [role: borrower]
reject(){ terminate() }

// The lender transfers the sum to the borrower
@signed [role: lender]
loan_start(){
  transferHoldings(lender, currency, principle, borrower);
  transitionTo(@contract_active)
}

// The borrower pays interest
@contract_active [role: borrower]
pay_interest(){
  interest_payment = round(2,principle * interest_rate);
  transferHoldings(borrower, currency, interest_payment, lender);
  stay()
}

// The borrower pays back the principle
@contract_active [role: borrower]
payback(){
  transferHoldings(lender, currency, principle, borrower);
  terminate()
}
