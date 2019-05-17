// Deadlock and livelock, taken from http://mlwiki.org/index.php/Workflow_Soundness#Situations_to_Avoid
@initial
go1() { transitionTo(@{p1, p5}) }

@p1
go2() { transitionTo(@p2) }

@p1
go3() { transitionTo(@p3) }

@{p2, p3}
go4() { transitionTo(@p7) }

@p5
go5() { transitionTo(@p6) }

@p6
go6() { transitionTo(@p5) }

@{p6, p7}
go7() { transitionTo(@terminal) }

