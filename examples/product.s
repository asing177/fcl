// This  workflow models a situation where two parties trade a SP smart contract
// Quanto Autocall Phoenix Note on the Worst of Two Stocks with Memory Coupon
global account p1;
global account p2;
global account xP;
global asset<decimal<4>> productCurrency;
global asset<decimal<4>> underlying1;
global asset<decimal<4>> underlying2;
global datetime strikeDate;
global decimal<4> numberOfCoupons;
global decimal<4> daysToCoupon;
global decimal<4> daysToMaturity;
global decimal<4> notional;
global int memoryCoupon;
global decimal<4> strikeLevel;
global decimal<4> couponBarrierLevel;
global decimal<4> autocallTriggerLevel;
global decimal<4> earlyRedemptionMultiplier;
global decimal<4> targetRate;
global decimal<4> targetRedemptionMultiplierFixing;
global decimal<4> targetRedemptionMultiplierStrike;
global decimal<4> dayCounter;
global decimal<4> couponsUnpaid;
global decimal<4> couponRate;
global decimal<4> redemptionStockPrice;
global decimal<4> fixingUnderlying1;
global decimal<4> fixingUnderlying2;
global decimal<4> whichCouponPeriod;
global asset<decimal<4>> assetRedeemed;
global decimal<4> couponValue;
global decimal<4> earlyRepayment;
global decimal<4> stockRepayment;
global decimal<4> repayment;

@initial
propose(account proposedP2, account proposedXP, asset<decimal<4>> proposedProductCurrency, asset<decimal<4>> proposedUnderlying1, asset<decimal<4>> proposedUnderlying2, datetime proposedStrikeDate, decimal<4> proposedStrikeLevel, decimal<4> proposedNumberOfCoupons, decimal<4> proposedDaysToCoupon, decimal<4> proposedDaysToMaturity, decimal<4> proposedNotional, int proposedMemoryCoupon, decimal<4> proposedCouponBarrierLevel, decimal<4> proposedAutocallTriggerLevel, decimal<4> proposedCouponRate, decimal<4> proposedEarlyRedemptionMultiplier, decimal<4> proposedRedemptionMultiplierFixing, decimal<4> proposedRedemptionMultiplierStrike) {
    p1 = sender();
    p2 = proposedP2;
    xP = proposedXP;
    productCurrency = proposedProductCurrency;
    underlying1 = proposedUnderlying1;
    underlying2 = proposedUnderlying2;
    strikeDate = proposedStrikeDate;
    strikeLevel = proposedStrikeLevel;
    numberOfCoupons = proposedNumberOfCoupons;
    daysToCoupon = proposedDaysToCoupon;
    daysToMaturity = proposedDaysToMaturity;
    notional = proposedNotional;
    memoryCoupon = proposedMemoryCoupon;
    couponBarrierLevel = proposedCouponBarrierLevel;
    autocallTriggerLevel = proposedAutocallTriggerLevel;
    targetRate = proposedCouponRate;
    earlyRedemptionMultiplier = proposedEarlyRedemptionMultiplier;
    targetRedemptionMultiplierFixing = proposedRedemptionMultiplierFixing;
    targetRedemptionMultiplierStrike = proposedRedemptionMultiplierStrike;
    transitionTo(:onOffer);
}
@onOffer [roles: {P2}]
accept() {
    if (now() < strikeDate) {
        dayCounter = 0.0000;
        couponRate = 0.0000;
        couponsUnpaid = 0.0000;
        redemptionStockPrice = 0.0000;
        couponValue = 0.0000;
        earlyRepayment = 0.0000;
        stockRepayment = 0.0000;
        repayment = 0.0000;
        whichCouponPeriod = 1.0000;
        transitionTo(:fixing);
    } else {
        transitionTo(:terminal);
    }
}

@fixing [roles: {xP}]
fix(decimal<4> observedPrice1, decimal<4> observedPrice2) {
    fixingUnderlying1 = observedPrice1;
    fixingUnderlying2 = observedPrice2;
    transitionTo(:registered);
}
@registered
lifecycle() {
    dayCounter = (dayCounter + 1.0000);
    if ((dayCounter == (whichCouponPeriod * daysToCoupon))) {
        transitionTo(:couponObservation);
    } else {
      if ((dayCounter == daysToMaturity)) {
          transitionTo(:finalObservation);
      } else {
          stay();
      };
    }
}

@couponObservation [roles: {xP}]
observeForCoupon(decimal<4> observedPrice1, decimal<4> observedPrice2) {
    if ((((observedPrice1 / fixingUnderlying1) > autocallTriggerLevel) && ((observedPrice2 / fixingUnderlying2) > autocallTriggerLevel))) {
      transitionTo(:earlyCall);
    } else {
        if ((((observedPrice1 / fixingUnderlying1) > couponBarrierLevel) && ((observedPrice2 / fixingUnderlying2) > couponBarrierLevel))) {
            couponRate = targetRate;
            transitionTo(:couponSettlement);
        } else {
            if ((memoryCoupon == 1)) {
                couponsUnpaid = (couponsUnpaid + 1.0000);
            };
            if ((whichCouponPeriod < numberOfCoupons)) {
                whichCouponPeriod = (whichCouponPeriod + 1.0000);
            };
            transitionTo(:registered);
        };
    };
}
@earlyCall [roles: {p1,p2}]
callSettlement() {
    couponValue = 0.0000;
    earlyRepayment = (notional * earlyRedemptionMultiplier);
    transitionTo(:terminal);
}
@couponSettlement [roles: {p1,p2}]
settleCoupon() {
    couponValue = ((notional * (couponsUnpaid + 1.0000)) * couponRate * (daysToCoupon/365.0000));
    couponRate = 0.0000;
    couponsUnpaid = 0.0000;
    if ((whichCouponPeriod < numberOfCoupons)) {
        whichCouponPeriod = (whichCouponPeriod + 1.0000);
    };
    transitionTo(:registered);
}
@finalObservation [roles: {xP}]
observeForMaturity(decimal<4> observedPrice1, decimal<4> observedPrice2, decimal<4> observedFX) {
    if ((((observedPrice1 / fixingUnderlying1) > 1.0000) && ((observedPrice2 / fixingUnderlying2) > 1.0000))) {
        transitionTo(:repayCashFixing);
    } else {
        if ((((observedPrice1 / fixingUnderlying1) > strikeLevel) && ((observedPrice2 / fixingUnderlying2) > strikeLevel))) {
            transitionTo(:repayCashStrike);
        } else {
            assetRedeemed = underlying2;
            if (((observedPrice1 / fixingUnderlying1) > (observedPrice2 / fixingUnderlying2))) {
                redemptionStockPrice = ((fixingUnderlying2 / observedFX));
            } else {
                assetRedeemed = underlying1;
                redemptionStockPrice = ((fixingUnderlying1 / observedFX));
            };
            transitionTo(:repayStock);
        };
    };
}
@repayCashFixing [roles: {p1, p2}]
settleCashAtMaturityFixing() {
    repayment = (notional * targetRedemptionMultiplierFixing);
    transitionTo(:terminal);
}
@repayCashStrike [roles: {p1, p2}]
settleCashAtMaturityStrike() {
    repayment = (notional * targetRedemptionMultiplierStrike);
    transitionTo(:terminal);
}
@repayStock [roles: {p1, p2}]
settleStockAtMaturity() {
    stockRepayment = (notional / redemptionStockPrice);
    transitionTo(:terminal);
}
@registered [roles: {p2}]
investorTransfer(account proposedTransferTo, decimal<4> proposedTransferAmount) {
    if ((notional > proposedTransferAmount)) {
        notional = (notional - proposedTransferAmount);
    } else {
        notional = 0.0000;
    };
    stay();
}
@registered [roles: {p1}]
issuerNovation(account proposedTransferTo, decimal<4> proposedTransferAmount) {
    if ((notional > proposedTransferAmount)) {
        notional = (notional - proposedTransferAmount);
    } else {
        notional = 0.0000;
    };
    stay();
}
