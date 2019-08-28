global int x;

@initial
split()
{
  transitionTo(@{a1,b});
}

@a1
init1()
{
  x = 0;
  transitionTo(@a2);
}

@{a2,b}
join()
{
  transitionTo(@commonState);
}

@initial
init2()
{
  x = 0;
  transitionTo(@commonState);
}

@commonState
use()
{
  y = x;
  terminate();
}
