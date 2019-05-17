// Deadlock (XOR-split followed by AND-join)
@initial
go1() { transitionTo(@a) }

@initial
go2() { transitionTo(@b) }

@{a, b}
go3() { transitionTo(@terminal) }

