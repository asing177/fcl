// If you're at `a`, you're basically done, but if you go to `b`, then you must go to `c`
@initial
go1() { transitionTo(@a) }

@a
go2() { transitionTo(@b) }

@b
go3() { transitionTo(@c) }

@c
go4() { transitionTo(@a) }

@a
go5() { transitionTo(@terminal) }

