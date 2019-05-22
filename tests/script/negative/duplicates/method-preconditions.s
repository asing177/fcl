// duplicate preconditions should be rejected
@initial [role : sender(), role : sender()]
foo() { transitionTo(@terminal) }
