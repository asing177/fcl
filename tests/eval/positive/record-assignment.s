type addr { Addr(text street, text city) }
type person { Pers(text name, addr address) }

global person bob = Pers("bob", Addr("main st", "springfield"));

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
