// Boundedness violation, taken partly from http://mlwiki.org/index.php/Workflow_Soundness#Boundness [sic]
@initial
go1() { transitionTo(@{p1, p2}) }

@p1
go2() { transitionTo(@initial) }

@p2
go3() { transitionTo(@initial) }

@{p1, p2}
go4() { transitionTo(@terminal) }

