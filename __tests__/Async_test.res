open Jest

@module("fs") @val
external readFile: (string, @as("utf-8") _, (. Js.Nullable.t<Js.Exn.t>, string) => unit) => unit =
  "readFile"

exception TestException

describe("Async", () => {
  open Expect
  let shouldError = (m, done) =>
    m |> Async.callback(result => {
      switch result {
      | Ok(_) => fail("expected error")
      | _ => pass
      } |> done
    })

  let shouldEqual = (x, m, done) =>
    m |> Async.callback(result => {
      ()
      done(expect(result)->toEqual(Ok(x)))
    })

  describe("err", () => testAsync("err", Async.err("sorry") |> shouldError))

  describe("exn", () => testAsync("exn", Async.exn(TestException) |> shouldError))

  describe("unit", () => {
    testAsync(
      "unit",
      done =>
        Async.unit(7) |> Async.callback(
          result => {
            done(expect(result)->toEqual(Ok(7)))
          },
        ),
    )

    testAsync(
      "unit 2",
      done =>
        Async.unit(7) |> Async.callback(
          result => {
            done(expect(result)->toEqual(Ok(7)))
          },
        ),
    )

    testAsync("unit 3", Async.unit(7) |> shouldEqual(7))
  })

  describe("rescript", () => {
    let readFile = filename => readFile(filename) |> Async.rescript
    testAsync(
      "readfile should work",
      readFile("__tests__/test-data.txt")
      |> Async.map(Js.String.trim)
      |> shouldEqual("chancellor on brink"),
    )

    testAsync("readfile should error on nonexistent file", readFile("doesnt exist") |> shouldError)
  })

  describe("map", () => {
    testAsync("map", Async.unit(49) |> Async.map(n => n / 7) |> shouldEqual(7))
    testAsync(
      "map with error",
      done => {
        Async.unit(49)
        |> Async.map(n => n / 0)
        |> Async.callback(
          result => {
            switch result {
            | Ok(_) => fail("OK not expected")
            | Error(_) => pass
            } |> done
          },
        )
      },
    )
    testAsync("map with error v2", Async.unit(49) |> Async.map(n => n / 0) |> shouldError)
  })

  describe("flatMap", () => {
    testAsync(
      "flatMap",
      Async.unit(49)
      |> Async.flatMap(n => Async.unit(n / 7))
      |> Async.map(n => n + 3)
      |> Async.flatMap(n => Async.unit(n / 5))
      |> Async.flatMap(n => Async.delay(100, n * 17))
      |> shouldEqual(34),
    )
  })

  describe("delay", () => {
    testAsync("delayed", Async.delay(100, "delayed") |> shouldEqual("delayed"))
    testAsync(
      "further delayed",
      Async.delay(200, "further delayed") |> shouldEqual("further delayed"),
    )
    testAsync(
      "delay doesnt timeout",
      Async.delay(100, "delayed") |> Async.timeout(200) |> shouldEqual("delayed"),
    )
    testAsync(
      "delay doesnt timeout",
      Async.delay(100, "delayed") |> Async.timeout(10) |> shouldError,
    )
  })

  describe("tuple", () => {
    testAsync(
      "tuple",
      (Async.delay(100, "a"), Async.unit(1)) |> Async.tuple |> shouldEqual(("a", 1)),
    )

    testAsync(
      "tuple3",
      Async.tuple3((
        Async.unit("a"),
        Async.delay(10, 1),
        Async.unit(1) |> Async.map(x => x == 1),
      )) |> shouldEqual(("a", 1, true)),
    )
    testAsync(
      "tuple4",
      (
        Async.delay(10, 1) |> Async.map(n => n + 2),
        Async.unit("a"),
        Async.delay(10, 1),
        Async.unit(1) |> Async.map(x => x == 1),
      )
      |> Async.tuple4
      |> shouldEqual((3, "a", 1, true)),
    )
  })

  describe("race", () => {
    testAsync(
      "race",
      (i => Async.delay((i + 1) * 100, i + 1))
      |> Belt.Array.makeBy(10)
      |> Belt.Array.shuffle
      |> Async.race
      |> shouldEqual(1),
    )

    testAsync(
      "delay with race",
      Async.race([Async.err("fail fast"), Async.delay(100, "success")]) |> shouldError,
    )
    testAsync(
      "delay with race",
      Async.race([
        Async.delay(200, 1) |> Async.map(x => x / 0),
        Async.delay(100, 1) |> Async.map(x => x / 1),
      ]) |> shouldEqual(1),
    )
  })

  describe("recover", () => {
    testAsync(
      "recover",
      Async.unit(1) |> Async.map(x => x / 0) |> Async.recover(_ => Async.unit(1)) |> shouldEqual(1),
    )
    testAsync(
      "recover but unnecessary",
      Async.unit(10)
      |> Async.map(x => x / 5)
      |> Async.recover(_ => Async.unit(1))
      |> shouldEqual(2),
    )
  })

  describe("timeout", () => {
    testAsync("timeout", Async.timeout(100, Async.delay(200, "just in time")) |> shouldError)

    testAsync(
      "timeout",
      Async.timeout(100, Async.delay(50, "just in time")) |> shouldEqual("just in time"),
    )
  })

  describe("parallel", () => {
    testAsync(
      "parallel times out",
      Async.parallel([Async.unit(10), Async.delay(500, 20), Async.delay(1000, 30)])
      |> Async.timeout(600)
      |> shouldError,
    )
    testAsync(
      "parallel doesnt timeout",
      Async.parallel([Async.unit(10), Async.delay(500, 20), Async.delay(1000, 30)])
      |> Async.timeout(1100)
      |> shouldEqual([10, 20, 30]),
    )
    testAsync("parallel works with an empty list", Async.parallel([]) |> shouldEqual([]))
  })

  describe("series", () => {
    testAsync(
      "series times out",
      Async.series([Async.unit(10), Async.delay(500, 20), Async.delay(1000, 30)])
      |> Async.timeout(1100)
      |> shouldError,
    )
    testAsync(
      "series doesnt timeout",
      Async.series([Async.unit(10), Async.delay(500, 20), Async.delay(1000, 30)])
      |> Async.timeout(1600)
      |> shouldEqual([10, 20, 30]),
    )
    testAsync("series works with an empty list", Async.series([]) |> shouldEqual([]))
  })

  describe("asyncify", () => {
    let f = Async.asyncify(x => x / 0)
    let g = Async.asyncify(x => x / 2)
    testAsync("asyncify errors", Async.unit(10) |> Async.flatMap(f) |> shouldError)
    testAsync("asyncify doesnt error", Async.unit(10) |> Async.flatMap(g) |> shouldEqual(5))
  })

  describe("fromPromise", () => {
    let p = Js.Promise.resolve(7)
    testAsync("fromPromise", Async.fromPromise(p) |> shouldEqual(7))
  })
  exception Badness
  describe("fromPromise with error", () => {
    let p = Js.Promise.reject(Badness) |> Async.fromPromise
    testAsync("fromPromise", p |> shouldError)
  })
})
