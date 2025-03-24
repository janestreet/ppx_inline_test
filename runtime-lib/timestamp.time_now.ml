type t = Base.Intt63.t

(* This function returns an int63 representing the number of nanos since
   some (fixed) baseline.  On unix, this baseline will be the unix epoch,
   and in javascript, the baseline will be "program initialization time."
   Regardless, it's always safe to subtract two values and use the diff,
   which is all that ppx_inline_test_lib uses it for. *)
let get () = Time_now.nanosecond_counter_for_timing ()
let seconds_since before = Base.Int63.(get () - before |> to_float) /. 1e9
