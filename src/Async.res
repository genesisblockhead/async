@new external makeError: string => Js.Exn.t = "Error"

type result<'a> = Belt.Result.t<'a, exn>
type null<'a> = Js.Nullable.t<'a>
type nodeback<'a> = (null<Js.Exn.t>, 'a) => unit
type callback<'a> = result<'a> => unit
type t<'a> = callback<'a> => unit

let once: callback<'a> => callback<'a> = cb => {
  let called = ref(false)
  x => {
    if !called.contents {
      called := true
      cb(x)
    }
  }
}

@val external setImmediate: ('a => unit, 'a) => unit = "setImmediate"
let unit: 'a => t<'a> = (x, cb) => setImmediate(cb, Ok(x))

let err = (message, cb) => {
  Js.Global.setTimeout(() => cb(Error(makeError(message) |> Js.Exn.anyToExnInternal)), 0) |> ignore
}
let asyncify: ('a => 'b, 'a) => t<'b> = (f, x, cb) =>
  try {
    Belt.Result.Ok(f(x))
  } catch {
  | e => Belt.Result.Error(e)
  } |> cb

@scope("process") external nextTick: (unit => unit) => unit = "nextTick"

let asyncRef = () => {
  let x = ref(false)
  nextTick(() => x := true)
  x
}

let callback: (callback<'a>, t<'a>) => unit = (cb, m) => {
  let ticked = asyncRef()
  m(result => {
    if ticked.contents {
      cb(result)
    } else {
      setImmediate(cb, result)
    }
  })
}

let map: ('a => 'b, t<'a>) => t<'b> = (f, m, cb) => {
  m |> callback(x => {
    switch x {
    | Error(e) => Error(e)
    | Ok(x) =>
      try {
        let y = f(x)
        Ok(y)
      } catch {
      | e => Error(e)
      }
    } |> cb
  })
}

let flatMap: ('a => t<'b>, t<'a>) => t<'b> = (f: 'a => t<'b>, m: t<'a>, cb: callback<'b>) =>
  m(x => {
    switch x {
    | Error(e) => cb(Error(e))
    | Ok(m) => f(m, cb)
    }
  })

let rescript = (f, cb) =>
  f((e, r) =>
    cb(
      switch Js.Nullable.toOption(e) {
      | None => Ok(r)
      | Some(e) => Error(Js.Exn.anyToExnInternal(e))
      },
    )
  )

@val external delay: ('a => unit, int, 'a) => unit = "setTimeout"
let delay: (int, 'a) => t<'a> = (n, x, cb) => delay(cb, n, Ok(x))

let tuple: ((t<'a>, t<'b>)) => t<('a, 'b)> = ((ma, mb), cb) => {
  let a = ref(None)
  let b = ref(None)
  let cb = once(cb)
  let go = () => {
    switch (a.contents, b.contents) {
    | (Some(Ok(a)), Some(Ok(b))) => cb(Ok((a, b)))
    | (Some(Error(e)), _) => cb(Error(e))
    | (_, Some(Error(e))) => cb(Error(e))
    | _ => ()
    }
  }
  ma(result => {
    switch a.contents {
    | None => a := Some(result)
    | _ => ()
    }
    go()
  })
  mb(result => {
    switch b.contents {
    | None => b := Some(result)
    | _ => ()
    }
    go()
  })
}

let tuple3: ((t<'a>, t<'b>, t<'c>)) => t<('a, 'b, 'c)> = ((ma, mb, mc)) =>
  tuple((tuple((ma, mb)), mc)) |> map((((a, b), c)) => (a, b, c))

let tuple4: ((t<'a>, t<'b>, t<'c>, t<'d>)) => t<('a, 'b, 'c, 'd)> = ((ma, mb, mc, md)) =>
  tuple((tuple((ma, mb)), tuple((mc, md)))) |> map((((a, b), (c, d))) => (a, b, c, d))

let parallel: array<t<'a>> => t<array<'a>> = (tasks, cb) => {
  if Js.Array.length(tasks) == 0 {
    cb(Ok([]))
  } else {
    let cb = once(cb)
    let finished = ref(0)
    let results = tasks |> Js.Array.map(_ => None)
    tasks |> Js.Array.forEachi((m, i) =>
      m(
        once(result => {
          switch result {
          | Ok(_) => {
              finished := finished.contents + 1
              results[i] = Some(result)
              if finished.contents == Js.Array.length(tasks) {
                cb(
                  Ok(
                    results |> Js.Array.map(result =>
                      switch result {
                      | Some(Ok(x)) => x
                      | _ => Js.Exn.raiseError("this is impossible dont worry")
                      }
                    ),
                  ),
                )
              }
            }
          | Error(e) => cb(Error(e))
          }
        }),
      )
    )
  }
}

let series: array<t<'a>> => t<array<'a>> = (tasks, cb) => {
  let length = Js.Array.length(tasks)
  let results = []
  let rec helper = (i, cb) => {
    if i < length {
      tasks[i](result => {
        switch result {
        | Error(e) => cb(Error(e))
        | Ok(x) => {
            results |> Js.Array.push(x) |> ignore
            helper(i + 1, cb)
          }
        }
      })
    } else {
      cb(Ok(results))
    }
  }
  helper(0, cb)
}

let race: array<t<'a>> => t<'a> = (tasks, cb) => {
  if Js.Array.length(tasks) == 0 {
    err("no tasks means nobody can win the race", cb)
  }
  let cb = once(cb)
  tasks |> Js.Array.forEach(callback(cb))
}

let recover = (handler, m, cb) => {
  m(r => {
    switch r {
    | Ok(x) => cb(Ok(x))
    | Error(e) => handler(e) |> callback(cb)
    }
  })
}

let timeout = (t, m, cb) => {
  let cb = once(cb)
  let timeout = Js.Global.setTimeout(() => {
    err("Timeout")(cb)
  }, t)
  m(x => {
    Js.Global.clearTimeout(timeout)
    cb(x)
  })
}
