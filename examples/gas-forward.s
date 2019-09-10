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

transition initial -> matching;
transition matching -> nomination;
transition matching -> terminal;
transition matching -> discrepancy;
transition discrepancy -> discrepancy;
transition discrepancy -> nomination;
transition discrepancy -> terminal;
transition nomination -> {volActualisation, priceActualisation};
transition nomination -> nomination;
transition volActualisation -> volActualisation;
transition volActualisation -> volActualisationDone;
transition priceActualisation -> priceActualisationDone;
transition {volActualisationDone, priceActualisationDone} -> ppaPeriod;
transition ppaPeriod -> ppaPeriod;
transition ppaPeriod -> terminal;

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
  nominatedQuantity = nominatedQuantity + nominatedQuantity_;
  if (counter < numberOfDeliveryDays + 1) {
    counter = counter + 1;
  };
  if (counter == numberOfDeliveryDays + 1) {
    counter = 1;
    transitionTo(@{volActualisation, priceActualisation});
  } else {
    stay();
  };
}

@volActualisation [role: pipeline]
actualisevolume(decimal<2> deliveredQuantity_) {
  deliveredQuantity = deliveredQuantity + deliveredQuantity_;
  if (counter < numberOfDeliveryDays + 1) {
    counter = counter + 1;
  };
  if (counter == numberOfDeliveryDays + 1) {
    counter = 1;
    transitionTo(@volActualisationDone);
  } else {
    stay();
  };
}

@priceActualisation [role: admin]
actualiseprice(decimal<2> price_) {
  price = price_;
  payment = round(2, deliveredQuantity * price);
  transitionTo(@priceActualisationDone);
}

@{volActualisationDone, priceActualisationDone} [role: buyer]
settle() {
  transferHoldings(buyer, asset_, payment, seller);
  transitionTo(@ppaPeriod);
}

@ppaPeriod [role: pipeline]
adjust(decimal<2> adjustedQuantity_) {
  adjustedQuantity = adjustedQuantity + adjustedQuantity_;
  if (counter < numberOfDeliveryDays + 1) {
    counter = counter + 1;
  };
  if (counter == numberOfDeliveryDays + 1) {
    ppa = round(2, price * adjustedQuantity) - payment;
    if (ppa > 0.00) {
      transferHoldings(buyer, asset_, ppa, seller);
      ppaPayer = buyer;
    };
    if (ppa < 0.00) {
      ppa = (0.00 - ppa);
      transferHoldings(seller, asset_, ppa, buyer);
      ppaPayer = seller;
    };
    transitionTo(@terminal);
  } else {
    stay();
  };
}
