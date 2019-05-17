// 8 Haskell programmers walk into a pub. Everybody needs to order, in any order
@initial
go1() { transitionTo(@{a1, b1, c1, d1, e1, f1, g1}) }

@a1
go2() { transitionTo(@a2) }

@b1
go3() { transitionTo(@b2) }

@c1
go4() { transitionTo(@c2) }

@d1
go5() { transitionTo(@d2) }

@e1
go6() { transitionTo(@e2) }

@f1
go7() { transitionTo(@f2) }

@g1
go8() { transitionTo(@g2) }

@{a2, b2, c2, d2, e2, f2, g2}
go10() { transitionTo(@terminal) }

