open Graph
open Tools

type 'a dijk = 
  { value: 'a ;
    cost: int }

type path = string
  
(* Return true ifn the arc has a positive remaining capacity. *)
let arc_flowable arc = if (arc.lbl.value>0) then true else false

let dijk_add_arc gr id1 id2 n = 
  let value1 = 
    match find_arc gr id1 id2 with 
    | None -> (match find_arc gr id2 id1 with
      | None -> failwith "Impossible"
      | Some arc -> {value=0;cost=(-arc.lbl.cost)})
    | Some arc -> arc.lbl
  in 
    new_arc gr {src=id1; tgt=id2; lbl={value=(value1.value+n);cost=value1.cost}}

let busackergowen graph origin destination =

  (* The algorithm fails if either Origin or Destination are not in the graph. *)
  if (not (node_exists graph origin) || not (node_exists graph destination)) then failwith "Id origine ou destination non existant" ;

  let rec dijk_mainloop graph3 = 

    let marked = origin in 
    let hashmarked = Hashtbl.create 12 in 
    n_iter graph3 (fun id -> Hashtbl.add hashmarked id (false, max_int, []));
    Hashtbl.replace hashmarked marked (true,0,[]);

    let rec dijk_find_path graph1 lastmarked =

      (* If destination is marked, then stop the search. *)
      let (bool1, _, _) = Hashtbl.find hashmarked destination in

      if bool1 then true
      else (

        let (_,currentmarkedcost,path) = Hashtbl.find hashmarked lastmarked in

        let outarcs = out_arcs graph1 lastmarked in 

        let rec updatenodescost = function
          | [] -> ()
          | arc::rest -> if arc_flowable arc then
            let (bool2, nodecost1, _path1) = Hashtbl.find hashmarked arc.tgt in
            let newcost = currentmarkedcost+arc.lbl.cost in
            if (bool2 || nodecost1<=newcost) then ()
            else Hashtbl.replace hashmarked arc.tgt (false,newcost,arc.tgt::path); updatenodescost rest
          else updatenodescost rest

        in 
        updatenodescost outarcs;
        let (_cost,nodetomark,path) = n_fold graph1 (fun (currentcost,currentnode,currentpath) id -> let (bool,cost,path) = Hashtbl.find hashmarked id in
          if (bool || cost<=currentcost) then (currentcost,currentnode,currentpath)
          else (cost,id,path)) (max_int,0,[]) in 
        
        Hashtbl.replace hashmarked nodetomark (true,nodetomark,path);
        
        dijk_find_path graph1 nodetomark
      )

    in

    let bool = dijk_find_path graph3 marked in

    if bool then (

      let (_,_,path) = Hashtbl.find hashmarked destination in 

      let rec find_flow inid maxflow = function
        | [] -> maxflow
        | x::rest -> match find_arc graph3 inid x with
          | None -> failwith "L'impossible est arrivé"
          | Some y -> find_flow x (min maxflow y.lbl.value) rest
      
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
        | x::rest -> let graph2 = dijk_add_arc graph2 inid x (-maxflow) in

          (* Increase the remaining capacity of the backward_arc. *)
          let graph2 = dijk_add_arc graph2 x inid (maxflow) in 
          add_flow graph2 x rest

      in 
      let graph3 = add_flow graph3 origin path in
      dijk_mainloop graph3

    ) else (
      graph3
    )

  in
  let ecartgraph = dijk_mainloop graph in

  (* Compute the resulted flow graph containing only the arcs in the initial graph. *)
  e_fold graph (fun gr arc1 -> match find_arc ecartgraph arc1.src arc1.tgt with

    (* If the result graph does not contain the arc, then this arc does not have any flow. *)
    | None -> let arc2 = {arc1 with lbl = {value=0;cost=arc1.lbl.cost}} in new_arc gr arc2

    (* Otherwise, the flow of the arc is the remaining capacity of the result arc - its total capacity.
     * The resulted flow must be above 0 *)
    | Some x -> let arc2 = {arc1 with lbl = {value=(max 0 (arc1.lbl.value-x.lbl.value));cost=arc1.lbl.cost}} in new_arc gr arc2) (clone_nodes graph)

