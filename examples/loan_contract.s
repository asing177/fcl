fixed2 principle;
fixed2 interest_rate;
assetFrac2 currency;
account borrower;
account lender;
text loan_contract;

@initial
propose_contract(fixed2 PRINCIPLE, assetFrac2 CURRENCY, account BORROWER, account LENDER, fixed2 INTEREST_RATE){
  borrower = BORROWER;
  lender = LENDER;
  principle = PRINCIPLE;
  currency = CURRENCY;
  interest_rate = INTEREST_RATE;
  transitionTo(@negotiate_terms)
}

// The lender gives their terms and conditions
@negotiate_terms [role: lender]
propose_terms(text LOAN_CONTRACT){
  loan_contract = LOAN_CONTRACT;
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
  interest_payment = (principle * interest_rate);
  transferHoldings(borrower, currency, interest_payment, lender);
  stay()
}

// The borrower pays back the principle
@contract_active [role: borrower]
payback(){
  transferHoldings(lender, currency, principle, borrower);
  terminate()
}
