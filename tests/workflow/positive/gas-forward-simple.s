@initial
start() {
  transitionTo(@nomination)
}

@nomination
nominate() {
  if (1 == 1) {
    transitionTo(@{preVolActualisation, priceActualisation});
  } else {
    stay();
  };
}

@preVolActualisation
preTransition() {
  transitionTo(@volActualisation)
}

@volActualisation
actualisevolume() {
  if (1 == 1) {
    transitionTo(@volActualisationDone);
  } else {
    transitionTo(@preVolActualisation);
  };
}

@priceActualisation
actualiseprice() {
  transitionTo(@priceActualisationDone);
}

@{volActualisationDone, priceActualisationDone}
settle() {
  transitionTo(@ppaPeriod);
}

@ppaPeriod
final() {
  terminate()
}
