type Address = Addr(text street, text city);
type Person = Pers(text name, Address address);

Person bob = Pers("bob", Addr("main st", "springfield"));

@initial
updateName(text newName) {
  bob.name = newName;
  stay()
}

@initial
updateStreet(text newStreet) {
  bob.address.street = newStreet;
  stay()
}

@initial
finish() {
  terminate()
}
