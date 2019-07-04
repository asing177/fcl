type Address = Addr(text street, int houseNumber, text city);
type Person = Pers(text name, Address address);

Person bob = Pers("bob", Addr("main st", 42, "springfield"));
int n;

@initial
go() {
  n = bob.address.houseNumber; // expecting 42
  terminate()
}
