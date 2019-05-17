// Do `a` as many times as you want before finishing
@initial
go1() { transitionTo(@a) }

@a
go2() { transitionTo(@a) }

@a
go3() { transitionTo(@terminal) }

