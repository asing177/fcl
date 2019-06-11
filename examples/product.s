// This  workflow models a situation where two parties trade a SP smart contract
// Quanto Autocall Phoenix Note on the Worst of Two Stocks with Memory Coupon
global account P1;
global account P2;
global account XP;
global assetFrac4 productCurrency;
global assetFrac4 underlying1;
global assetFrac4 underlying2;
global datetime strikeDate;
global fixed4 numberOfCoupons;
global fixed4 daysToCoupon;
global fixed4 daysToMaturity;
global fixed4 notional;
global int memoryCoupon;
global fixed4 strikeLevel;
global fixed4 couponBarrierLevel;
global fixed4 autocallTriggerLevel;
global fixed4 earlyRedemptionMultiplier;
global fixed4 targetRate;
global fixed4 targetRedemptionMultiplierFixing;
global fixed4 targetRedemptionMultiplierStrike;
global fixed4 dayCounter;
global fixed4 couponsUnpaid;
global fixed4 couponRate;
global fixed4 redemptionStockPrice;
global fixed4 fixingUnderlying1;
global fixed4 fixingUnderlying2;
global fixed4 whichCouponPeriod;
global assetFrac4 assetRedeemed;
global fixed4 couponValue;
global fixed4 earlyRepayment;
global fixed4 stockRepayment;
global fixed4 repayment;

@initial
propose(account proposedP2, account proposedXP, assetFrac4 proposedProductCurrency, assetFrac4 proposedUnderlying1, assetFrac4 proposedUnderlying2, datetime proposedStrikeDate, fixed4 proposedStrikeLevel, fixed4 proposedNumberOfCoupons, fixed4 proposedDaysToCoupon, fixed4 proposedDaysToMaturity, fixed4 proposedNotional, int proposedMemoryCoupon, fixed4 proposedCouponBarrierLevel, fixed4 proposedAutocallTriggerLevel, fixed4 proposedCouponRate, fixed4 proposedEarlyRedemptionMultiplier, fixed4 proposedRedemptionMultiplierFixing, fixed4 proposedRedemptionMultiplierStrike) {
    P1 = sender();
    P2 = proposedP2;
    XP = proposedXP;
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
        dayCounter = 0.0000f;
        couponRate = 0.0000f;
        couponsUnpaid = 0.0000f;
        redemptionStockPrice = 0.0000f;
        couponValue = 0.0000f;
        earlyRepayment = 0.0000f;
        stockRepayment = 0.0000f;
        repayment = 0.0000f;
        whichCouponPeriod = 1.0000f;
        transitionTo(:fixing);
    } else {
        transitionTo(:terminal);
    }
}

@fixing [roles: {XP}]
fix(fixed4 observedPrice1, fixed4 observedPrice2) {
    fixingUnderlying1 = observedPrice1;
    fixingUnderlying2 = observedPrice2;
    transitionTo(:registered);
}
@registered
lifecycle() {
    dayCounter = (dayCounter + 1.0000f);
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

@couponObservation [roles: {XP}]
observeForCoupon(fixed4 observedPrice1, fixed4 observedPrice2) {
    if ((((observedPrice1 / fixingUnderlying1) > autocallTriggerLevel) && ((observedPrice2 / fixingUnderlying2) > autocallTriggerLevel))) {
      transitionTo(:earlyCall);
    } else {
        if ((((observedPrice1 / fixingUnderlying1) > couponBarrierLevel) && ((observedPrice2 / fixingUnderlying2) > couponBarrierLevel))) {
            couponRate = targetRate;
            transitionTo(:couponSettlement);
        } else {
            if ((memoryCoupon == 1)) {
                couponsUnpaid = (couponsUnpaid + 1.0000f);
            };
            if ((whichCouponPeriod < numberOfCoupons)) {
                whichCouponPeriod = (whichCouponPeriod + 1.0000f);
            };
            transitionTo(:registered);
        };
    };
}
@earlyCall [roles: {P1,P2}]
callSettlement() {
    couponValue = 0.0000f;
    earlyRepayment = (notional * earlyRedemptionMultiplier);
    transitionTo(:terminal);
}
@couponSettlement [roles: {P1,P2}]
settleCoupon() {
    couponValue = ((notional * (couponsUnpaid + 1.0000f)) * couponRate * (daysToCoupon/365.0000f));
    couponRate = 0.0000f;
    couponsUnpaid = 0.0000f;
    if ((whichCouponPeriod < numberOfCoupons)) {
        whichCouponPeriod = (whichCouponPeriod + 1.0000f);
    };
    transitionTo(:registered);
}
@finalObservation [roles: {XP}]
observeForMaturity(fixed4 observedPrice1, fixed4 observedPrice2, fixed4 observedFX) {
    if ((((observedPrice1 / fixingUnderlying1) > 1.0000f) && ((observedPrice2 / fixingUnderlying2) > 1.0000f))) {
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
@repayCashFixing [roles: {P1, P2}]
settleCashAtMaturityFixing() {
    repayment = (notional * targetRedemptionMultiplierFixing);
    transitionTo(:terminal);
}
@repayCashStrike [roles: {P1, P2}]
settleCashAtMaturityStrike() {
    repayment = (notional * targetRedemptionMultiplierStrike);
    transitionTo(:terminal);
}
@repayStock [roles: {P1, P2}]
settleStockAtMaturity() {
    stockRepayment = (notional / redemptionStockPrice);
    transitionTo(:terminal);
}
@registered [roles: {P2}]
investorTransfer(account proposedTransferTo, fixed4 proposedTransferAmount) {
    if ((notional > proposedTransferAmount)) {
        notional = (notional - proposedTransferAmount);
    } else {
        notional = 0.0000f;
    };
    stay();
}
@registered [roles: {P1}]
issuerNovation(account proposedTransferTo, fixed4 proposedTransferAmount) {
    if ((notional > proposedTransferAmount)) {
        notional = (notional - proposedTransferAmount);
    } else {
        notional = 0.0000f;
    };
    stay();
}
