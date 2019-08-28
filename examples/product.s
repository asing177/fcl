// This workflow models a situation where two parties trade a SP smart contract
// Quanto Autocall Phoenix Note on the Worst of Two Stocks with Memory Coupon
global account p1;
global account p2;
global account xP;
global asset<decimal<4>> productCurrency;
global asset<int> underlying1;
global asset<int> underlying2;
global datetime strikeDate;
global int numberOfCoupons;
global int daysToCoupon;
global int daysToMaturity;
global decimal<4> notional; // amount of money
global bool memoryCoupon;
global decimal<4> strikeLevel;
global decimal<4> couponBarrierLevel;
global decimal<4> autocallTriggerLevel;
global decimal<4> earlyRedemptionMultiplier;
global decimal<4> targetRate;
global decimal<4> targetRedemptionMultiplierFixing;
global decimal<4> targetRedemptionMultiplierStrike;
global int dayCounter;
global int couponsUnpaid;
global decimal<4> couponRate;
global decimal<4> redemptionStockPrice;
global decimal<4> fixingUnderlying1;
global decimal<4> fixingUnderlying2;
global int whichCouponPeriod;
global asset<int> assetRedeemed;
global decimal<4> couponValue;
global decimal<4> earlyRepayment;
global int stockRepayment; // qty of asset redeemed
global decimal<4> remainderRepayment;
global decimal<4> repayment;

@initial
propose(
    account proposedP2,
    account proposedXP,
    asset<decimal<4>> proposedProductCurrency,
    asset<int> proposedUnderlying1,
    asset<int> proposedUnderlying2,
    datetime proposedStrikeDate,
    decimal<4> proposedStrikeLevel,
    int proposedNumberOfCoupons,
    int proposedDaysToCoupon,
    int proposedDaysToMaturity,
    decimal<4> proposedNotional,
    bool proposedMemoryCoupon,
    decimal<4> proposedCouponBarrierLevel,
    decimal<4> proposedAutocallTriggerLevel,
    decimal<4> proposedCouponRate,
    decimal<4> proposedEarlyRedemptionMultiplier,
    decimal<4> proposedRedemptionMultiplierFixing,
    decimal<4> proposedRedemptionMultiplierStrike
) {
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
    transitionTo(@onOffer);
}

@onOffer [role: p2, before: strikeDate]
accept() {
    dayCounter = 0;
    couponRate = 0;
    couponsUnpaid = 0;
    redemptionStockPrice = 0;
    couponValue = 0;
    earlyRepayment = 0;
    stockRepayment = 0;
    repayment = 0;
    whichCouponPeriod = 1;
    transitionTo(@fixing);
}

@onOffer [role: p2, after: strikeDate]
lapse() {
    terminate()
}

@fixing [role: xP]
fix(decimal<4> observedPrice1, decimal<4> observedPrice2) {
    fixingUnderlying1 = observedPrice1;
    fixingUnderlying2 = observedPrice2;
    transitionTo(@registered);
}

@registered
lifecycle() {
    dayCounter = dayCounter + 1;
    if (dayCounter == (whichCouponPeriod * daysToCoupon)) {
        transitionTo(@couponObservation);
    } else if (dayCounter == daysToMaturity) {
        transitionTo(@finalObservation);
    } else {
        stay();
    }
}

@couponObservation [role: xP]
observeForCoupon(decimal<4> observedPrice1, decimal<4> observedPrice2) {
    if ((((observedPrice1 / fixingUnderlying1) > autocallTriggerLevel) && ((observedPrice2 / fixingUnderlying2) > autocallTriggerLevel))) {
        transitionTo(@earlyCall);
    } else if ((((observedPrice1 / fixingUnderlying1) > couponBarrierLevel) && ((observedPrice2 / fixingUnderlying2) > couponBarrierLevel))) {
        couponRate = targetRate;
        transitionTo(@couponSettlement);
    } else {
        if (memoryCoupon) {
            couponsUnpaid = couponsUnpaid + 1;
        };
        if (whichCouponPeriod < numberOfCoupons) {
            whichCouponPeriod = whichCouponPeriod + 1;
        };
        transitionTo(@registered);
    }
}

@earlyCall [roles: {p1,p2}]
callSettlement() {
    couponValue = 0;
    earlyRepayment = round(4, notional * earlyRedemptionMultiplier);
    transitionTo(@terminal);
}

@couponSettlement [roles: {p1,p2}]
settleCoupon() {
    couponValue = round(4, ((notional * (couponsUnpaid + 1)) * couponRate * (daysToCoupon/365)));
    couponRate = 0;
    couponsUnpaid = 0;
    if ((whichCouponPeriod < numberOfCoupons)) {
        whichCouponPeriod = (whichCouponPeriod + 1);
    };
    transitionTo(@registered);
}

@finalObservation [roles: {xP}]
observeForMaturity(decimal<4> observedPrice1, decimal<4> observedPrice2, decimal<4> observedFX) {
    if ((((observedPrice1 / fixingUnderlying1) > 1) && ((observedPrice2 / fixingUnderlying2) > 1))) {
        transitionTo(@repayCashFixing);
    } else if ((((observedPrice1 / fixingUnderlying1) > strikeLevel) && ((observedPrice2 / fixingUnderlying2) > strikeLevel))) {
        transitionTo(@repayCashStrike);
    } else {
        assetRedeemed = underlying2;
        if (((observedPrice1 / fixingUnderlying1) > (observedPrice2 / fixingUnderlying2))) {
            redemptionStockPrice = round(4, (fixingUnderlying2 / observedFX));
        } else {
            assetRedeemed = underlying1;
            redemptionStockPrice = round(4, (fixingUnderlying1 / observedFX));
        };
        transitionTo(@repayStock);
    }
}

@repayCashFixing [roles: {p1, p2}]
settleCashAtMaturityFixing() {
    repayment = round(4, notional * targetRedemptionMultiplierFixing);
    transitionTo(@terminal);
}

@repayCashStrike [roles: {p1, p2}]
settleCashAtMaturityStrike() {
    repayment = round(4, notional * targetRedemptionMultiplierStrike);
    transitionTo(@terminal);
}

@repayStock [roles: {p1, p2}]
settleStockAtMaturity() {
    stockRepayment = round(0, notional / redemptionStockPrice);
    remainderRepayment = round(4, roundRem(0, notional / redemptionStockPrice));
    transitionTo(@terminal);
}

@registered [roles: {p2}]
investorTransfer(account proposedTransferTo, decimal<4> proposedTransferAmount) {
    if ((notional > proposedTransferAmount)) {
        notional = (notional - proposedTransferAmount);
    } else {
        notional = 0;
    };
    stay();
}

@registered [roles: {p1}]
issuerNovation(account proposedTransferTo, decimal<4> proposedTransferAmount) {
    if ((notional > proposedTransferAmount)) {
        notional = (notional - proposedTransferAmount);
    } else {
        notional = 0;
    };
    stay();
}
