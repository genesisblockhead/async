// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Jest = require("@glennsl/rescript-jest/src/jest.bs.js");
var Async = require("../src/Async.bs.js");
var Curry = require("rescript/lib/js/curry.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");

Jest.describe("Async", (function (param) {
        var shouldError = function (m, done) {
          Async.callback((function (result) {
                  var tmp;
                  tmp = result.TAG === /* Ok */0 ? Jest.fail("expected error") : Jest.pass;
                  Curry._1(done, tmp);
                }), m);
        };
        var shouldEqual = function (x, m, done) {
          Async.callback((function (result) {
                  Curry._1(done, Jest.Expect.toEqual(Jest.Expect.expect(result), {
                            TAG: /* Ok */0,
                            _0: x
                          }));
                }), m);
        };
        Jest.describe("err", (function (param) {
                Jest.testAsync("err", undefined, (function (param) {
                        return shouldError((function (param) {
                                      return Async.err("sorry", param);
                                    }), param);
                      }));
              }));
        Jest.describe("unit", (function (param) {
                Jest.testAsync("unit", undefined, (function (done) {
                        Async.callback((function (result) {
                                Curry._1(done, Jest.Expect.toEqual(Jest.Expect.expect(result), {
                                          TAG: /* Ok */0,
                                          _0: 7
                                        }));
                              }), (function (param) {
                                return Async.unit(7, param);
                              }));
                      }));
                Jest.testAsync("unit 2", undefined, (function (done) {
                        Async.callback((function (result) {
                                Curry._1(done, Jest.Expect.toEqual(Jest.Expect.expect(result), {
                                          TAG: /* Ok */0,
                                          _0: 7
                                        }));
                              }), (function (param) {
                                return Async.unit(7, param);
                              }));
                      }));
                Jest.testAsync("unit 3", undefined, (function (param) {
                        return shouldEqual(7, (function (param) {
                                      return Async.unit(7, param);
                                    }), param);
                      }));
              }));
        Jest.describe("rescript", (function (param) {
                var partial_arg = function (param) {
                  return Async.rescript((function (param) {
                                Fs.readFile("README.md", "utf-8", param);
                              }), param);
                };
                var partial_arg$1 = function (param) {
                  return Async.map((function (prim) {
                                return prim.trim();
                              }), partial_arg, param);
                };
                Jest.testAsync("readfile should work", undefined, (function (param) {
                        return shouldEqual("todo: add readme", partial_arg$1, param);
                      }));
                var partial_arg$2 = function (param) {
                  return Async.rescript((function (param) {
                                Fs.readFile("doesnt exist", "utf-8", param);
                              }), param);
                };
                Jest.testAsync("readfile should error on nonexistent file", undefined, (function (param) {
                        return shouldError(partial_arg$2, param);
                      }));
              }));
        Jest.describe("map", (function (param) {
                Jest.testAsync("map", undefined, (function (param) {
                        return shouldEqual(7, (function (param) {
                                      return Async.map((function (n) {
                                                    return n / 7 | 0;
                                                  }), (function (param) {
                                                    return Async.unit(49, param);
                                                  }), param);
                                    }), param);
                      }));
                Jest.testAsync("map with error", undefined, (function (done) {
                        Async.callback((function (result) {
                                var tmp;
                                tmp = result.TAG === /* Ok */0 ? Jest.fail("OK not expected") : Jest.pass;
                                Curry._1(done, tmp);
                              }), (function (param) {
                                return Async.map((function (n) {
                                              return Caml_int32.div(n, 0);
                                            }), (function (param) {
                                              return Async.unit(49, param);
                                            }), param);
                              }));
                      }));
                Jest.testAsync("map with error v2", undefined, (function (param) {
                        return shouldError((function (param) {
                                      return Async.map((function (n) {
                                                    return Caml_int32.div(n, 0);
                                                  }), (function (param) {
                                                    return Async.unit(49, param);
                                                  }), param);
                                    }), param);
                      }));
              }));
        Jest.describe("flatMap", (function (param) {
                Jest.testAsync("flatMap", undefined, (function (param) {
                        return shouldEqual(34, (function (param) {
                                      return Async.flatMap((function (n) {
                                                    var partial_arg = Math.imul(n, 17);
                                                    return function (param) {
                                                      return Async.delay(100, partial_arg, param);
                                                    };
                                                  }), (function (param) {
                                                    return Async.flatMap((function (n) {
                                                                  var partial_arg = n / 5 | 0;
                                                                  return function (param) {
                                                                    return Async.unit(partial_arg, param);
                                                                  };
                                                                }), (function (param) {
                                                                  return Async.map((function (n) {
                                                                                return n + 3 | 0;
                                                                              }), (function (param) {
                                                                                return Async.flatMap((function (n) {
                                                                                              var partial_arg = n / 7 | 0;
                                                                                              return function (param) {
                                                                                                return Async.unit(partial_arg, param);
                                                                                              };
                                                                                            }), (function (param) {
                                                                                              return Async.unit(49, param);
                                                                                            }), param);
                                                                              }), param);
                                                                }), param);
                                                  }), param);
                                    }), param);
                      }));
              }));
        Jest.describe("delay", (function (param) {
                Jest.testAsync("delayed", undefined, (function (param) {
                        return shouldEqual("delayed", (function (param) {
                                      return Async.delay(100, "delayed", param);
                                    }), param);
                      }));
                Jest.testAsync("further delayed", undefined, (function (param) {
                        return shouldEqual("further delayed", (function (param) {
                                      return Async.delay(200, "further delayed", param);
                                    }), param);
                      }));
                Jest.testAsync("delay doesnt timeout", undefined, (function (param) {
                        return shouldEqual("delayed", (function (param) {
                                      return Async.timeout(200, (function (param) {
                                                    return Async.delay(100, "delayed", param);
                                                  }), param);
                                    }), param);
                      }));
                Jest.testAsync("delay doesnt timeout", undefined, (function (param) {
                        return shouldError((function (param) {
                                      return Async.timeout(10, (function (param) {
                                                    return Async.delay(100, "delayed", param);
                                                  }), param);
                                    }), param);
                      }));
              }));
        Jest.describe("tuple", (function (param) {
                var partial_arg_0 = function (param) {
                  return Async.delay(100, "a", param);
                };
                var partial_arg_1 = function (param) {
                  return Async.unit(1, param);
                };
                var partial_arg = [
                  partial_arg_0,
                  partial_arg_1
                ];
                var partial_arg$1 = function (param) {
                  return Async.tuple(partial_arg, param);
                };
                var partial_arg$2 = [
                  "a",
                  1
                ];
                Jest.testAsync("tuple", undefined, (function (param) {
                        return shouldEqual(partial_arg$2, partial_arg$1, param);
                      }));
                var partial_arg$3 = Async.tuple3([
                      (function (param) {
                          return Async.unit("a", param);
                        }),
                      (function (param) {
                          return Async.delay(10, 1, param);
                        }),
                      (function (param) {
                          return Async.map((function (x) {
                                        return x === 1;
                                      }), (function (param) {
                                        return Async.unit(1, param);
                                      }), param);
                        })
                    ]);
                var partial_arg$4 = [
                  "a",
                  1,
                  true
                ];
                Jest.testAsync("tuple3", undefined, (function (param) {
                        return shouldEqual(partial_arg$4, partial_arg$3, param);
                      }));
                var partial_arg$5 = Async.tuple4([
                      (function (param) {
                          return Async.map((function (n) {
                                        return n + 2 | 0;
                                      }), (function (param) {
                                        return Async.delay(10, 1, param);
                                      }), param);
                        }),
                      (function (param) {
                          return Async.unit("a", param);
                        }),
                      (function (param) {
                          return Async.delay(10, 1, param);
                        }),
                      (function (param) {
                          return Async.map((function (x) {
                                        return x === 1;
                                      }), (function (param) {
                                        return Async.unit(1, param);
                                      }), param);
                        })
                    ]);
                var partial_arg$6 = [
                  3,
                  "a",
                  1,
                  true
                ];
                Jest.testAsync("tuple4", undefined, (function (param) {
                        return shouldEqual(partial_arg$6, partial_arg$5, param);
                      }));
              }));
        Jest.describe("race", (function (param) {
                var partial_arg = Belt_Array.shuffle(Belt_Array.makeBy(10, (function (i) {
                            var partial_arg = i + 1 | 0;
                            var partial_arg$1 = Math.imul(i + 1 | 0, 100);
                            return function (param) {
                              return Async.delay(partial_arg$1, partial_arg, param);
                            };
                          })));
                var partial_arg$1 = function (param) {
                  return Async.race(partial_arg, param);
                };
                Jest.testAsync("race", undefined, (function (param) {
                        return shouldEqual(1, partial_arg$1, param);
                      }));
                var partial_arg$2 = [
                  (function (param) {
                      return Async.err("fail fast", param);
                    }),
                  (function (param) {
                      return Async.delay(100, "success", param);
                    })
                ];
                var partial_arg$3 = function (param) {
                  return Async.race(partial_arg$2, param);
                };
                Jest.testAsync("delay with race", undefined, (function (param) {
                        return shouldError(partial_arg$3, param);
                      }));
                var partial_arg$4 = [
                  (function (param) {
                      return Async.map((function (x) {
                                    return Caml_int32.div(x, 0);
                                  }), (function (param) {
                                    return Async.delay(200, 1, param);
                                  }), param);
                    }),
                  (function (param) {
                      return Async.map((function (x) {
                                    return x / 1 | 0;
                                  }), (function (param) {
                                    return Async.delay(100, 1, param);
                                  }), param);
                    })
                ];
                var partial_arg$5 = function (param) {
                  return Async.race(partial_arg$4, param);
                };
                Jest.testAsync("delay with race", undefined, (function (param) {
                        return shouldEqual(1, partial_arg$5, param);
                      }));
              }));
        Jest.describe("recover", (function (param) {
                Jest.testAsync("recover", undefined, (function (param) {
                        return shouldEqual(1, (function (param) {
                                      return Async.recover((function (param) {
                                                    return function (param) {
                                                      return Async.unit(1, param);
                                                    };
                                                  }), (function (param) {
                                                    return Async.map((function (x) {
                                                                  return Caml_int32.div(x, 0);
                                                                }), (function (param) {
                                                                  return Async.unit(1, param);
                                                                }), param);
                                                  }), param);
                                    }), param);
                      }));
                Jest.testAsync("recover but unnecessary", undefined, (function (param) {
                        return shouldEqual(2, (function (param) {
                                      return Async.recover((function (param) {
                                                    return function (param) {
                                                      return Async.unit(1, param);
                                                    };
                                                  }), (function (param) {
                                                    return Async.map((function (x) {
                                                                  return x / 5 | 0;
                                                                }), (function (param) {
                                                                  return Async.unit(10, param);
                                                                }), param);
                                                  }), param);
                                    }), param);
                      }));
              }));
        Jest.describe("timeout", (function (param) {
                Jest.testAsync("timeout", undefined, (function (param) {
                        return shouldError((function (param) {
                                      return Async.timeout(100, (function (param) {
                                                    return Async.delay(200, "just in time", param);
                                                  }), param);
                                    }), param);
                      }));
                Jest.testAsync("timeout", undefined, (function (param) {
                        return shouldEqual("just in time", (function (param) {
                                      return Async.timeout(100, (function (param) {
                                                    return Async.delay(50, "just in time", param);
                                                  }), param);
                                    }), param);
                      }));
              }));
        Jest.describe("parallel", (function (param) {
                var partial_arg = [
                  (function (param) {
                      return Async.unit(10, param);
                    }),
                  (function (param) {
                      return Async.delay(500, 20, param);
                    }),
                  (function (param) {
                      return Async.delay(1000, 30, param);
                    })
                ];
                var partial_arg$1 = function (param) {
                  return Async.parallel(partial_arg, param);
                };
                var partial_arg$2 = function (param) {
                  return Async.timeout(600, partial_arg$1, param);
                };
                Jest.testAsync("parallel times out", undefined, (function (param) {
                        return shouldError(partial_arg$2, param);
                      }));
                var partial_arg$3 = [
                  (function (param) {
                      return Async.unit(10, param);
                    }),
                  (function (param) {
                      return Async.delay(500, 20, param);
                    }),
                  (function (param) {
                      return Async.delay(1000, 30, param);
                    })
                ];
                var partial_arg$4 = function (param) {
                  return Async.parallel(partial_arg$3, param);
                };
                var partial_arg$5 = function (param) {
                  return Async.timeout(1100, partial_arg$4, param);
                };
                var partial_arg$6 = [
                  10,
                  20,
                  30
                ];
                Jest.testAsync("parallel doesnt timeout", undefined, (function (param) {
                        return shouldEqual(partial_arg$6, partial_arg$5, param);
                      }));
                var partial_arg$7 = [];
                var partial_arg$8 = function (param) {
                  return Async.parallel(partial_arg$7, param);
                };
                var partial_arg$9 = [];
                Jest.testAsync("parallel works with an empty list", undefined, (function (param) {
                        return shouldEqual(partial_arg$9, partial_arg$8, param);
                      }));
              }));
        Jest.describe("series", (function (param) {
                var partial_arg = [
                  (function (param) {
                      return Async.unit(10, param);
                    }),
                  (function (param) {
                      return Async.delay(500, 20, param);
                    }),
                  (function (param) {
                      return Async.delay(1000, 30, param);
                    })
                ];
                var partial_arg$1 = function (param) {
                  return Async.series(partial_arg, param);
                };
                var partial_arg$2 = function (param) {
                  return Async.timeout(1100, partial_arg$1, param);
                };
                Jest.testAsync("series times out", undefined, (function (param) {
                        return shouldError(partial_arg$2, param);
                      }));
                var partial_arg$3 = [
                  (function (param) {
                      return Async.unit(10, param);
                    }),
                  (function (param) {
                      return Async.delay(500, 20, param);
                    }),
                  (function (param) {
                      return Async.delay(1000, 30, param);
                    })
                ];
                var partial_arg$4 = function (param) {
                  return Async.series(partial_arg$3, param);
                };
                var partial_arg$5 = function (param) {
                  return Async.timeout(1600, partial_arg$4, param);
                };
                var partial_arg$6 = [
                  10,
                  20,
                  30
                ];
                Jest.testAsync("series doesnt timeout", undefined, (function (param) {
                        return shouldEqual(partial_arg$6, partial_arg$5, param);
                      }));
                var partial_arg$7 = [];
                var partial_arg$8 = function (param) {
                  return Async.series(partial_arg$7, param);
                };
                var partial_arg$9 = [];
                Jest.testAsync("series works with an empty list", undefined, (function (param) {
                        return shouldEqual(partial_arg$9, partial_arg$8, param);
                      }));
              }));
        Jest.describe("asyncify", (function (param) {
                var f = function (param, param$1) {
                  return Async.asyncify((function (x) {
                                return Caml_int32.div(x, 0);
                              }), param, param$1);
                };
                var g = function (param, param$1) {
                  return Async.asyncify((function (x) {
                                return x / 2 | 0;
                              }), param, param$1);
                };
                Jest.testAsync("asyncify errors", undefined, (function (param) {
                        return shouldError((function (param) {
                                      return Async.flatMap(f, (function (param) {
                                                    return Async.unit(10, param);
                                                  }), param);
                                    }), param);
                      }));
                Jest.testAsync("asyncify doesnt error", undefined, (function (param) {
                        return shouldEqual(5, (function (param) {
                                      return Async.flatMap(g, (function (param) {
                                                    return Async.unit(10, param);
                                                  }), param);
                                    }), param);
                      }));
              }));
      }));

/*  Not a pure module */
