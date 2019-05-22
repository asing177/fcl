global int [roles: {u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc'}] a;

transition initial -> terminal;

@initial [roles: {u'fwBVDsVh8SYQy98CzYpNPcbyTRczVUZ96HszhNRB8Ve'}]
init() {
  a = 10;
  terminate();
}
