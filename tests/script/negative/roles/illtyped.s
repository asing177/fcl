global int [roles: {"not valid", "also not valid"}] x;

transition initial -> terminal;

@initial
go() { transitionTo(@terminal) }
