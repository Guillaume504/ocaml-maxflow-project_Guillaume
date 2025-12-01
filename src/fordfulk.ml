open Graph
open Tools

type flot = int

(* renvoie true ssi l'arc a un flow admissible > 0 *)
let arc_flowable arc = if (arc.lbl>0) then true else false

(* graph is an int graph
   origin is an id
   destination is an id *)
let fordfulkerson graph origin destination = 

  (* method fails if either origin or destination is not an id of the graph *)
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
  
  (* find_path renvoie le tuple (boolean, id list) avec boolean true ssi la destination a été atteinte, et id list la liste des id des nodes constituant le chemin
     graph1 is an int graph
     idnode is the id of the current node
     acu is the initial list of node ids *)
  let rec find_path graph1 idnode acu =
    let outarcs = out_arcs graph1 idnode in
    if (List.exists (fun x -> (x.tgt == destination && arc_flowable x)) outarcs) then (true, List.rev (destination::acu))
    else (
      let rec iterarcs = function
        | [] -> (false, [])
        | arc::rest -> if (arc_flowable arc && not (List.exists (fun x -> if (x == arc.tgt) then true else false) acu) && arc.tgt != origin) then 
          let (res,path) = find_path graph1 (arc.tgt) (arc.tgt::acu) in
            if res then (res,path) else iterarcs rest
          else iterarcs rest
      in
        iterarcs outarcs
    )
  in

  let rec mainloop graph3 =
    let (bool, path) = find_path graph3 origin [] in
    if bool then (
      (* find_flow renvoie la valeur du flow max admissible sur le chemin trouvé *)
      let rec find_flow inid maxflow = function
        | [] -> maxflow
        | x::rest -> match find_arc graph3 inid x with
          | None -> failwith "L'impossible est arrivé"
          | Some y -> find_flow x (min maxflow y.lbl) rest
      in 
      let maxflow = find_flow origin max_int path in
      Printf.printf "flowmax = %d\n%!" maxflow;
      let pathprint = List.map string_of_int path in
      let pathprint = String.concat " -> " pathprint in 
      Printf.printf "Path is : %s\n\n%!" pathprint;
      (* add_flow ajoute la valeur maxflow au flow de chaque arc du chemin donné *)
      let rec add_flow graph2 inid = function
        | [] -> graph2
        | x::rest -> let graph2 = add_arc graph2 inid x (-maxflow) in
          let graph2 = add_arc graph2 x inid (maxflow) in 
          add_flow graph2 x rest
    in 
      let graph3 = add_flow graph3 origin path in
      mainloop graph3
    ) else (
      graph3
    )
  in
  let ecartgraph = mainloop graph in
  e_fold graph (fun gr arc1 -> match find_arc ecartgraph arc1.src arc1.tgt with
    | None -> let arc2 = {arc1 with lbl = 0} in new_arc gr arc2
    | Some x -> let arc2 = {arc1 with lbl = (max 0 (arc1.lbl-x.lbl))} in new_arc gr arc2) (clone_nodes graph)
  
let fordfulkerson_string graph origin destination =
  let graph2 = fordfulkerson graph origin destination in
  e_fold graph2 (fun gr arc1 -> match find_arc graph arc1.src arc1.tgt with
    | None -> failwith "Impossible"
    | Some x -> let arc2 = {arc1 with lbl = Printf.sprintf "%d/%d" arc1.lbl x.lbl} in new_arc gr arc2) (clone_nodes graph)