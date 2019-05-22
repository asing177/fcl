// Can't reach terminal state from AND split
@initial
go1() { transitionTo(@a) }

@a
go2() { transitionTo(@terminal) }

@a
go3() { transitionTo(@{c, d}) }

@a
go4() { transitionTo(@b) }

@b
go5() { transitionTo(@a) }

