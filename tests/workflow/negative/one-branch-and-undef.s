global int x;

@initial
split()
{
  transitionTo(@{a1,b});
}

@a1
init()
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
noop()
{
  transitionTo(@commonState);
}

@commonState
use()
{
  y = x;
  terminate();
}
