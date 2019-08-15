global int x;

@initial
init1()
{
  x = 0;
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
