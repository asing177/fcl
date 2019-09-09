global account seller = u'6fgCTVBrVLiXxCdZZWBznabxwEebfu6iAP93S1s358Ca';
global account buyer;
global account admin = u'FoUAmBFu3vd3eHfqfurqpM7intjZMbXNLqzty5eKihBF';
global account pipeline = u'2vJ8JYN43hKwZx6p3r7fuRToZ3NiqGrPRngdq63hAskf';
global asset<decimal<2>> asset_ = a'EBYttLyWUzhtdhFyAp8nAuKXZ7UZjxDLbry2Bnd8KuA5';
global decimal<2> dailyQuantity;
global decimal<2> dailyQuantityCounter;
global decimal<2> nominatedQuantity = 0.00;
global decimal<2> deliveredQuantity = 0.00;
global decimal<2> adjustedQuantity = 0.00;
global text index;
global text pipelineName;
global text deliveryLocation;
global decimal<2> price;
global decimal<2> payment;
global decimal<2> ppa;
global datetime deliveryStartDate;
global int numberOfDeliveryDays;
global int counter = 1;
global account ppaPayer;

@initial [role: seller]
propose(account proposedBuyer, decimal<2> proposedDailyQuantity, text proposedPipelineName, text proposedDeliveryLocation, text proposedIndex, datetime proposedDeliveryStartDate, int proposedNumberOfDeliveryDays) {
  buyer = proposedBuyer;
  dailyQuantity = proposedDailyQuantity;
  pipelineName = proposedPipelineName;
  deliveryLocation = proposedDeliveryLocation;
  index = proposedIndex;
  deliveryStartDate = proposedDeliveryStartDate;
  numberOfDeliveryDays = proposedNumberOfDeliveryDays;
  transitionTo(@matching);
}

@matching [before: deliveryStartDate, role: buyer]
match(decimal<2> dailyQuantity_) {
  dailyQuantityCounter = dailyQuantity_;
  if ((dailyQuantity == dailyQuantityCounter)) {
    transitionTo(@nomination);
  } else {
    transitionTo(@discrepancy);
  }
}

@matching [after: deliveryStartDate]
endDuringMatching() {
  terminate()
}

@discrepancy [before: deliveryStartDate, role: seller]
resolve(decimal<2> dailyQuantity_) {
  dailyQuantityCounter = dailyQuantity_;
  if (dailyQuantity == dailyQuantityCounter) {
    transitionTo(@nomination);
  } else {
    transitionTo(@discrepancy);
  }
}

@discrepancy [after: deliveryStartDate]
endDuringDiscrepancy() {
  terminate()
}

@nomination [role: seller]
nominate(decimal<2> nominatedQuantity_) {
  terminate()
}

