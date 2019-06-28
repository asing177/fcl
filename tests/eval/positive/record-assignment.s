type Address = Addr(text street, text city);
type Person = Pers(text name, Address address);

Person bob = Pers("bob", Addr("main st", "springfield"));

@initial
go(text newStreet) {
  bob.address.street = newStreet;
  terminate()
}
