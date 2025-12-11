open Graph
open Tools

(* Return true ifn the arc has a positive remaining capacity. *)
let arc_flowable arc = if (arc.lbl>0) then true else false

let fordfulkerson graph origin destination = 

  (* The algorithm fails if either Origin or Destination are not in the graph. *)
  if (not (node_exists graph origin) || not (node_exists graph destination)) then failwith "Id origine ou destination non existant" ;
  
  (* Find a path between a node and the Destination node in the graph.
   * Return (true, path: id list) if found, (false, []) otherwise. *)
  let rec find_path graph1 idnode acu =

    (* Get the out_arcs of the current node. *)
    let outarcs = out_arcs graph1 idnode in

    (* If the Destination node can be reached from current node, then a path has been found. *)
    if (List.exists (fun x -> (x.tgt == destination && arc_flowable x)) outarcs) then (true, List.rev (destination::acu))

    else (

      (* Search the first flowable arc within out_arcs which destination is not already in the path. *)
      let rec iterarcs = function
        | [] -> (false, [])
        
        (* If the arc is flowable and does not lead to a loop. *)
        | arc::rest -> if (arc_flowable arc && not (List.exists (fun x -> if (x == arc.tgt) then true else false) acu) && arc.tgt != origin) then 

          (* Search for a path starting from the destination node of this arc. *)
          let (res,path) = find_path graph1 (arc.tgt) (arc.tgt::acu) in

          (* If a path has been found, stop the search.
           * Try with next arc in out_arcs otherwise *)
          if res then (res,path) else iterarcs rest
        else iterarcs rest

      in
      iterarcs outarcs
    )
  in

  (* Main loop of the algorithm, continue until no path can be found between the Origin and Destination nodes. *)
  let rec mainloop graph3 =

    (* Try to find a path between Origin and Destination nodes. *)
    let (bool, path) = find_path graph3 origin [] in

    (* If a path has been found. *)
    if bool then (

      (* Find the maximum allowable flow in the found path. *)
      let rec find_flow inid maxflow = function
        | [] -> maxflow
        | x::rest -> match find_arc graph3 inid x with
          | None -> failwith "L'impossible est arrivé"
          | Some y -> find_flow x (min maxflow y.lbl) rest
      
      in 
      let maxflow = find_flow origin max_int path in

      (* Print out the maximum flow found in the console. *)
      Printf.printf "flowmax = %d\n%!" maxflow;

      (* Print out the explored path in the console. *)
      let pathprint = List.map string_of_int path in
      let pathprint = String.concat " -> " pathprint in 
      Printf.printf "Path is : %s\n\n%!" pathprint;

      (* Add the maximum flow found to the arcs of the path. *)
      let rec add_flow graph2 inid = function
        | [] -> graph2

        (* Reduce the remaining capacity of the forward_arc due to its increased flow. *)
        | x::rest -> let graph2 = add_arc graph2 inid x (-maxflow) in

          (* Increase the remaining capacity of the backward_arc. *)
          let graph2 = add_arc graph2 x inid (maxflow) in 
          add_flow graph2 x rest

      in 
      let graph3 = add_flow graph3 origin path in
      mainloop graph3
    ) 

    (* If no path can be found, stop the algorithm. *)
    else (
      graph3
    )

  in
  let ecartgraph = mainloop graph in

  (* Compute the resulted flow graph containing only the arcs in the initial graph. *)
  e_fold graph (fun gr arc1 -> match find_arc ecartgraph arc1.src arc1.tgt with

    (* If the result graph does not contain the arc, then this arc does not have any flow. *)
    | None -> let arc2 = {arc1 with lbl = 0} in new_arc gr arc2

    (* Otherwise, the flow of the arc is the remaining capacity of the result arc - its total capacity.
     * The resulted flow must be above 0 *)
    | Some x -> let arc2 = {arc1 with lbl = (max 0 (arc1.lbl-x.lbl))} in new_arc gr arc2) (clone_nodes graph)

let bipartitematching graph = 
  
  let graph = fordfulkerson graph 0 (-1) in 

  (* Clone the graph nodes except Origin and Destination. *)
  let trimmedgraph = n_fold graph (fun gr i -> if i==0 || i==(-1) then gr else new_node gr i) empty_graph in

  (* Clone the graph arcs except those from the Origin and toward the Destination *)
  e_fold graph (fun gr arc -> if arc.src==0 || arc.tgt==(-1) then gr else add_arc gr arc.src arc.tgt arc.lbl) (trimmedgraph)