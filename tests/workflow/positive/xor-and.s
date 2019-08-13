// Jump between two mutually exclusive (XOR) concurrent (AND) branches with a shared place `b`
@initial
go1() { transitionTo(@{a, b}) }

@initial
go2() { transitionTo(@{b, c}) }

@{b, c}
go3() { transitionTo(@{a, b}) }

@{a, b}
go4() { transitionTo(@terminal) }

@{b, c}
go5() { transitionTo(@terminal) }

