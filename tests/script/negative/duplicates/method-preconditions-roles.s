// role and roles are synonyms, so this should be rejected
@initial [role : sender(), roles : sender()]
foo() { transitionTo(@terminal) }
