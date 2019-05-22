global account [roles: {bob}] bob; // unbound variable 'bob'

transition initial -> terminal;

@initial
go() { terminate(); }
