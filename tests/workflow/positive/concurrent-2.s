// Alice and Bob both need to do something in parallel before they can go to `c`. At `c` they can either start over or end
@initial
go1() { transitionTo(@{a1, b1}) }

@a1
go2() { transitionTo(@a2) }

@b1
go3() { transitionTo(@b2) }

@{a2, b2}
go4() { transitionTo(@c) }

@c
go5() { transitionTo(@initial) }

@c
go6() { transitionTo(@terminal) }

