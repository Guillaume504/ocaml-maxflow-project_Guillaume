
open Graph
open Fordfulk
open Printf
    
type path = string

let ensure graph id = if node_exists graph id then graph else new_node graph id

let bipartiteresolution path1 source sink =

  let hashtidname = Hashtbl.create 6 in

  let hashtnameid = Hashtbl.create 6 in

  let bi_from_file path =

    let infile = open_in path in

    (* Read all lines until end of file. *)
    let rec loop graph idpers =
      try
        let line = input_line infile in

        (* Remove leading and trailing spaces. *)
        let line = String.trim line in

        let (graph2,id) =
          (* Ignore empty lines *)
          if line = "" then (graph,idpers)

          else (
            let split_str = String.split_on_char ' ' line in

            match split_str with
              | [] -> failwith "strange case where there's a not empty line with nothing"
              | name::rest1 -> Hashtbl.add hashtidname idpers name; Hashtbl.add hashtnameid name idpers;
                let graph = new_arc (ensure (ensure graph 0) idpers) {src=0; tgt=idpers; lbl=1} in
                let rec loop2 graph3 destlist currentid = match destlist with
                  | [] -> (graph3, currentid)
                  | dest::rest2 -> try let idest = Hashtbl.find hashtnameid dest in
                      let graph3 = new_arc (ensure (ensure graph3 idpers) idest) {src=idpers; tgt=idest; lbl=1} in
                      loop2 graph3 rest2 currentid
                    with Not_found -> Hashtbl.add hashtnameid dest currentid; Hashtbl.add hashtidname currentid dest;
                      let graph3 = new_arc (ensure (ensure graph3 currentid) (-1)) {src=currentid; tgt=(-1); lbl=1} in
                      let graph3 = new_arc (ensure (ensure graph3 idpers) currentid) {src=idpers; tgt=currentid; lbl=1} in
                      loop2 graph3 rest2 (currentid+1)
                in
                loop2 graph rest1 (idpers+1)
          )
        in
        loop graph2 id

      with End_of_file -> graph (* Done *)
    in
    let final_graph = loop empty_graph 1 in
    
    close_in infile ;
    final_graph
  in
  let bi_export path2 graph4 =

    (* Open a write-file. *)
    let ff = open_out path2 in

    (* Write in this file. *)
    fprintf ff "digraph finite_state_machine {
    fontname=\"Helvetica,Arial,sans-serif\"
    node [fontname=\"Helvetica,Arial,sans-serif\"]
    edge [fontname=\"Helvetica,Arial,sans-serif\"]
    rankdir=LR;
    node [shape = circle];\n" ;

    (* Write all arcs *)
    let _ = e_fold graph4 (fun count arc -> if arc.src = 0 || arc.tgt = (-1) then (count + 1) else (fprintf ff "  %s -> %s [label = \"%s\"];\n" (Hashtbl.find hashtidname arc.src) (Hashtbl.find hashtidname arc.tgt) arc.lbl ; count + 1)) 0 in
    
    fprintf ff "}" ;
    
    close_out ff ;
    ()
  in
  let graph5 = bi_from_file path1 in 
  let graph5 = fordfulkerson_string graph5 source sink in
  bi_export "test.txt" graph5;
  graph5