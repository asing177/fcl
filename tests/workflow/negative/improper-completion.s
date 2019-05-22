// Improper completion (AND-split followed by XOR-join)
@initial
go1() { transitionTo(@{a, b}) }

@a
go2() { transitionTo(@terminal) }

@b
go3() { transitionTo(@terminal) }

