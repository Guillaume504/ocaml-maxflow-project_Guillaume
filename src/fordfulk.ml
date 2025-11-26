open Graph
open Tools

type flot = int

let arc_flowable arc = if (arc.lbl>0) then true else false

(* graph is an int graph
   origin is an id
   destination is an id *)
let fordfulkerson graph origin destination = 

  if (not (node_exists graph origin) || not (node_exists graph destination)) then failwith "Id origine ou destination non existant" ;

  (*let path_might_exist graph_pme = 
    let graph_out_arcs = out_arcs graph_pme origin in 
    if (graph_out_arcs == []) then false
    else (
      let rec loop graph_arcs = match graph_arcs with
        | [] -> false
        | x::rest1 -> if (x.lbl>0) then true else loop rest1
      in
        loop graph_out_arcs
    )
  in*)
  
  let rec find_path idnode acu =
    let outarcs = out_arcs graph idnode in
    if (List.exists (fun x -> (x.tgt == destination && arc_flowable x)) outarcs) then (true, destination::acu)
    else (
      let rec iterarcs = function
        | [] -> (false, [])
        | arc::rest -> if (arc_flowable arc) then 
          let (res,path) = find_path (arc.tgt) (arc.tgt::acu) in
            if res then (res,path) else iterarcs rest
        else iterarcs rest
      in
        iterarcs outarcs
    )
  in assert false