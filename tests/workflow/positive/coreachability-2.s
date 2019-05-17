// This test revealed that we were previously too strict about coreachability
@initial
go1() { transitionTo(@a) }

@a
go2() { transitionTo(@b) }

@b
go3() { transitionTo(@c) }

@c
go4() { transitionTo(@d) }

@d
go5() { transitionTo(@a) }

@b
go6() { transitionTo(@terminal) }

