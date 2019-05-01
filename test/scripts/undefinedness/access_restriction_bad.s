global account bob;

transition initial -> terminal;

@initial [roles: {bob}]
go() { terminate(); }
