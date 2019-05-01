global account seller = u'6fgCTVBrVLiXxCdZZWBznabxwEebfu6iAP93S1s358Ca';
global account buyer;
global account admin = u'FoUAmBFu3vd3eHfqfurqpM7intjZMbXNLqzty5eKihBF';
global account pipeline = u'2vJ8JYN43hKwZx6p3r7fuRToZ3NiqGrPRngdq63hAskf';
global assetFrac2 asset_ = a'EBYttLyWUzhtdhFyAp8nAuKXZ7UZjxDLbry2Bnd8KuA5';
global fixed2 dailyQuantity;
global fixed2 dailyQuantityCounter;
global fixed2 nominatedQuantity = 0.00f;
global fixed2 deliveredQuantity = 0.00f;
global fixed2 adjustedQuantity = 0.00f;
global text index;
global text pipelineName;
global text deliveryLocation;
global fixed2 price;
global fixed2 payment;
global fixed2 ppa;
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
propose(account proposedBuyer, fixed2 proposedDailyQuantity, text proposedPipelineName, text proposedDeliveryLocation, text proposedIndex, datetime proposedDeliveryStartDate, int proposedNumberOfDeliveryDays) {
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
match(fixed2 dailyQuantity_) {
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
resolve(fixed2 dailyQuantity_) {
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
nominate(fixed2 nominatedQuantity_) {
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
actualisevolume(fixed2 deliveredQuantity_) {
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
actualiseprice(fixed2 price_) {
  price = price_;
  payment = deliveredQuantity * price;
  transitionTo(@priceActualisationDone);
}

@{volActualisationDone, priceActualisationDone} [role: buyer]
settle() {
  transferHoldings(buyer, asset_, payment, seller);
  transitionTo(@ppaPeriod);
}

@ppaPeriod [role: pipeline]
adjust(fixed2 adjustedQuantity_) {
  adjustedQuantity = adjustedQuantity + adjustedQuantity_;
  if (counter < numberOfDeliveryDays + 1) {
    counter = counter + 1;
  };
  if (counter == numberOfDeliveryDays + 1) {
    ppa = (price * adjustedQuantity) - payment;
    if (ppa > 0.00f) {
      transferHoldings(buyer, asset_, ppa, seller);
      ppaPayer = buyer;
    };
    if (ppa < 0.00f) {
      ppa = (0.00f - ppa);
      transferHoldings(seller, asset_, ppa, buyer);
      ppaPayer = seller;
    };
    transitionTo(@terminal);
  } else {
    stay();
  };
}
