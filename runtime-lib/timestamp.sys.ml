type t = float

let get () = Sys.time ()
let seconds_since before = get () -. before
