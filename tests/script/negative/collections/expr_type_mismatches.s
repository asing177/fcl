map<int, text> foo = (1+1 : 15/3, 8*8 : "bar");
set<int> bar = {sender(), 99-99, 3.14};

transition initial -> terminal;

@initial
go() { transitionTo(@terminal); }
