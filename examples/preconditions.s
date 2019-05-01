transition initial -> terminal;

@initial [role: sender(), after: "2017-10-10T00:00:00Z"]
go() { terminate() }
