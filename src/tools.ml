(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr:'a graph) = n_fold gr (fun gra i -> new_node gra i) empty_graph

let gmap gr f = e_fold gr (fun gr1 arc1 -> let arc2 = {arc1 with lbl = (f arc1.lbl)} in new_arc gr1 arc2) (clone_nodes gr)

let add_arc gr id1 id2 n = 
  let value = 
    match find_arc gr id1 id2 with 
    | None -> 0
    | Some arc -> arc.lbl
  in 
    new_arc gr {src=id1; tgt=id2; lbl=(value+n)}