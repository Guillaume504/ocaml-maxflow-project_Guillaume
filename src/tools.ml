(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes _gr = assert false
let gmap _gr _f = assert false
(* Replace _gr and _f by gr and f when you start writing the real function. *)
let add_arc _gr _id1 _id2 _n = assert false