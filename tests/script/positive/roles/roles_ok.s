global account bob = u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc';

transition initial -> terminal;

@initial [roles: {bob, u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc'}]
init() {
  terminate();
}
