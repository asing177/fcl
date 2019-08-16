global int x;

@initial
init()
{
  x = 0;
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
