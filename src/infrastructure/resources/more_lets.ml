(** Additional [let] binders.

    We try to avoid these where possible (in preference for [lwt%let] as they do not preserve stack
    traces, but are sometimes useful.)*)

let ( let>> ) = Lwt_result.bind
