type t<'a>

type cb<'a> = (. Js.Nullable.t<Js.Exn.t>, 'a) => unit
type nodeback<'a> = (cb<'a>) => unit

let err: string => t<'a>
let unit: 'a => t<'a>

let rescript: nodeback<'a> => t<'a>

let asyncify: ('a => 'b) => ('a => t<'b>)
let map: ('a => 'b, t<'a>) => t<'b>
let flatMap: ('a => t<'b>, t<'a>) => t<'b>

let tuple: ((t<'a>, t<'b>)) => t<('a, 'b)>
let tuple3: ((t<'a>, t<'b>, t<'c>)) => t<('a, 'b, 'c)>
let tuple4: ((t<'a>, t<'b>, t<'c>, t<'d>)) => t<('a, 'b, 'c, 'd)>

let recover: (exn => t<'a>, t<'a>) => t<'a>

let delay: (int, 'a) => t<'a>
let race: array<t<'a>> => t<'a>
let timeout: (int, t<'a>) => t<'a>

let fromPromise: Js.Promise.t<'a> => t<'a>

let parallel: array<t<'a>> => t<array<'a>>
let series: array<t<'a>> => t<array<'a>>

let callback: (Belt.Result.t<'a, exn> => unit, t<'a>) => unit
let raiseIfError: t<'a> => unit
