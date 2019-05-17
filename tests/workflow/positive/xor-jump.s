// Jump between XOR threads
@initial
go1() { transitionTo(@a) }

@initial
go2() { transitionTo(@b) }

@a
go3() { transitionTo(@a) }

@a
go4() { transitionTo(@terminal) }

@b
go5() { transitionTo(@a) }

@b
go6() { transitionTo(@b) }

