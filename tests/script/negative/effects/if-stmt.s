@initial
init(asset<int> myAsset) {
  if (sha256(transferTo(myAsset, 100)) == sha256(true)) {
    terminate();
  } else {
    terminate();
  };
}
