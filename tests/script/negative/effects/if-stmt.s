@initial
init(asset<int> myAsset) {
  if (sha256(transferTo(myAsset, 100)) == sha256(True)) {
    terminate();
  } else {
    terminate();
  };
}