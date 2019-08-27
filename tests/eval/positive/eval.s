global decimal<1> m = 1.5;
global decimal<5> n = 10.00000;
global num w = 4.5;
global int x = 5;
global int y = 2;
global int z = 3;
global bool t = true;
global bool f = true;

global asset<bool> a = a'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';
global account b = u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';

global timedelta td = 1y1mo1d1h1m1s;
global datetime dt_future;
global datetime dt_past;
global timedelta future_past_diff;

transition initial -> terminal;

/* test basic bin ops on TCrypto/Tnum/TInt/TFixed/TDateTime/TDelta types */
/* TODO: Write comments above each test describing the binop tested */
@initial
f () {
  dt_ = now();
  dt_future = "2090-10-10T23:13:40+05:00";
  dt_past = "1999-02-23T23:13:40+05:00";
  dt_past_plus1 = dt_past + td;              /* add delta to datetimes */
  delta3 = (td * 2) + td;                    /* add and mult delta */
  dt_past = dt_past_plus1 + delta3;

  c = 1;
  d = y * c;
  e = 2.0 * w;
  w = e - w + 1.5;
  x = 2*x - x;
  z = d + y;
  y = y - z;
  y = round(0, x / y);
  w = w / 2.0;

  m = round(1, m * 2.0 / 3.0 - 5.0);
  n = -55.00000 + n;

  if (t || f) {
    t = false;
  } else {
    f = true;
  };

  before (dt_future) {
    x = x + 1;
  };
  before (dt_past) {
    y = y + 10000;
  };

  after (dt_future) {
    x = x + 10000;
  };
  after (dt_past) {
    y = y + 1;
  };

  between (dt_past, dt_future) {
    x = x * 3;
  };

  // test 'timeDiff' primop
  future_past_diff = timeDiff(dt_past, dt_future);

  terminate();
}
