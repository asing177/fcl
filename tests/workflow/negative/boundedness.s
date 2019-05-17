// Boundedness violation
@initial
go1() { transitionTo(@a) }

@a
go2() { transitionTo(@{a, b}) }

@b
go3() { transitionTo(@terminal) }

