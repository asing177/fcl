// Dead transition transition `b -> end`
@initial
go1() { transitionTo(@a) }

@a
go2() { transitionTo(@terminal) }

@b
go3() { transitionTo(@terminal) }

