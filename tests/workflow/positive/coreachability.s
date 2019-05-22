// This test revealed that we were previously too strict about coreachability
@initial
go1() { transitionTo(@a) }

@a
go2() { transitionTo(@b) }

@b
go3() { transitionTo(@a) }

@a
go4() { transitionTo(@c) }

@b
go5() { transitionTo(@c) }

@c
go6() { transitionTo(@terminal) }

