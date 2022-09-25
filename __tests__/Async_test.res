open Jest

describe("Async", () => {
  open Expect
  let shouldError = (m, done) =>
    m |> Async.callback(result => {
      switch result {
      | Ok(_) => fail("expected error")
      | _ => pass
      } |> done
    })

  let shouldEqual = (x, m, done) => {
    m |> Async.callback(result => {
      ()
      done(expect(result)->toEqual(Ok(x)))
    })
  }

  describe("unit", () => {
    testAsync("unit", done =>
      Async.unit(7, result => {
        done(expect(result)->toEqual(Ok(7)))
      })
    )

    testAsync("unit 2", done =>
      Async.unit(7) |> Async.callback(result => {
        done(expect(result)->toEqual(Ok(7)))
      })
    )

    testAsync("unit 3", Async.unit(7) |> shouldEqual(7))
  })

  describe("err", () => {
    testAsync("err", Async.err("sorry") |> shouldError)
  })

  describe("map", () => {
    testAsync("map", Async.unit(49) |> Async.map(n => n / 7) |> shouldEqual(7))
    testAsync("map with error", done => {
      Async.unit(49)
      |> Async.map(n => n / 0)
      |> Async.callback(result => {
        switch result {
        | Ok(_) => fail("OK not expected")
        | Error(_) => pass
        } |> done
      })
    })
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
})
