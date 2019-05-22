// Co-reachability violation
@initial
go1() { transitionTo(@a) }

@a
go2() { transitionTo(@a) }

