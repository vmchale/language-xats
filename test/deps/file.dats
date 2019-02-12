#prefix 99 !

(* /* *)

(* (* *) *)

datavtype list_vtype_int_vtbox(a: vtype+, int) =
  | list_vt_nil(a, 0) of ()
  | { n : int | n >= 0 } list_vt_cons(a, n+1) of (a, list_vtype_int_vtbox(a, n))

#staload /* comment */ "./file.sats"

#symload (* nested (*comment*) *) "prelude/fixity.sats"
