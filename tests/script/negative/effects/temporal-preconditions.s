global int [after:  "2018-10-10T00:00:00Z"] n;
global int [before: "2018-10-10T00:00:00Z"] m;
global datetime d = "2020-10-10T00:00:00Z";
global int [before: d] k;


transition initial -> terminal;

@initial [after: "2017-10-10T00:00:00Z", before: "2019-10-10T00:00:00Z"]
m1() {
  n = 1; // BAD
  m = 1; // BAD
  terminate()
}

@initial [before: d]
m2() {
  k = 1; // OK
  terminate()
}
