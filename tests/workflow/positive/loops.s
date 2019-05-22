// Do `a` with optional loops to `b` or `d`
@initial
go1() { transitionTo(@a) }

@a
go2() { transitionTo(@b) }

@b
go3() { transitionTo(@c) }

@c
go4() { transitionTo(@a) }

@a
go5() { transitionTo(@d) }

@d
go6() { transitionTo(@e) }

@e
go7() { transitionTo(@a) }

@a
go8() { transitionTo(@terminal) }

