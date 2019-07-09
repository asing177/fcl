type addr { Addr(text street, int houseNumber, text city) }
type person { Person(text name, addr address) }

global person bob = Person("bob", Addr("main st", 42, "springfield"));
global int n;

@initial
go() {
  n = bob.address.houseNumber; // expecting 42
  terminate()
}
