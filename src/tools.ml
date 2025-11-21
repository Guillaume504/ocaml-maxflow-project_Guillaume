(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr:'a graph) = n_fold gr (fun gra i -> new_node gra i) empty_graph

let gmap gr f = e_fold gr (fun gr1 arc1 -> let arc2 = {arc1 with lbl = (f arc1.lbl)} in new_arc gr1 arc2) (clone_nodes gr)

let add_arc _gr _id1 _id2 _n = assert false