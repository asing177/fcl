// An XOR branch in the beginning with the ability to jump between them at the end
@initial
go1() { transitionTo(@a1) }

@a1
go2() { transitionTo(@a2) }

@a2
go3() { transitionTo(@a3) }

@a3
go4() { transitionTo(@b) }

@a3
go5() { transitionTo(@terminal) }

@initial
go6() { transitionTo(@b) }

@b
go7() { transitionTo(@a3) }

@b
go8() { transitionTo(@terminal) }

