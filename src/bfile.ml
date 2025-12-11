open Printf
open Graph

type path = string

type bipartres = 
  { graph: int graph ;
    namefromid: (id -> string) }

(* Ensure that the given node exists in the graph. If not, create it. 
 * (Necessary because the website we use to create online graphs does not generate correct files when some nodes have been deleted.) *)
let ensure graph id = if node_exists graph id then graph else new_node graph id

let bi_write_file namefromid path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Search for an arc with a flow of 1.
   * Return (true, arc) if found, (false, a placeholder arc) otherwise. *)
  let rec iterarcs = function
    | [] -> (false,{src=(-2);tgt=(-2);lbl=(-2)})
    | arc::rest -> if arc.lbl == 1 then (true, arc) else iterarcs rest
  in

  (* Write all people's job. *)
  let _ = n_fold graph (fun count nodeid -> 

    (* Nodes with id<=0 are job nodes, thus ignored. *)
    (if nodeid<=0 then ()

    (* If the person has an arc with a flow of 1, the arc points toward the person's job. *)
    else let (bool,arc) = iterarcs (out_arcs graph nodeid) in
      if bool then fprintf ff "%s a eu le job %s\n" (namefromid nodeid) (namefromid arc.tgt)

      (* Otherwise the said person does not have a job. *)
      else fprintf ff "%s n'a pas eu de job" (namefromid nodeid)); count+1) 0 in
    
  close_out ff ;
  ()

let bi_from_file path =

  (* Hashtable containing the pairs (key=id,value=name). *)
  let hashtidname = Hashtbl.create 14 in

  (* Hashtable containing the pairs (key=name,value=id). *)
  let hashtnameid = Hashtbl.create 14 in

  (* Adding the Origin and Destination nodes in the hashtables. *)
  Hashtbl.add hashtidname 0 "Origin"; Hashtbl.add hashtnameid "Origin" 0;
  Hashtbl.add hashtidname (-1) "Dest"; Hashtbl.add hashtnameid "Dest" (-1);

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
          (* Split the different fields of a line in a string list. *)
          let split_str = String.split_on_char ' ' line in

          match split_str with
            | [] -> failwith "strange case where there's a not empty line with nothing"

            (* First argument of each line is the name of the person, which is added to both hashtables with id=idpers.
             * All person nodes have a positive id. *)
            | name::rest1 -> Hashtbl.add hashtidname idpers name; Hashtbl.add hashtnameid name idpers;

              (* Add an arc between the Origin node and the person node to the graph. *)
              let graph = new_arc (ensure (ensure graph 0) idpers) {src=0; tgt=idpers; lbl=1} in

              (* Iterate over all job of interest of the person. *)
              let rec loop2 graph3 destlist currentid = match destlist with
                | [] -> (graph3, currentid)

                (* If job node already exists in the hashtables. *)
                | dest::rest2 -> try let idest = Hashtbl.find hashtnameid dest in

                  (* Add an arc between the person node and the job node in the graph. *)
                  let graph3 = new_arc (ensure (ensure graph3 idpers) idest) {src=idpers; tgt=idest; lbl=1} in
                  loop2 graph3 rest2 currentid
                  
                  (* If job node does not exists in the hashtables, add it to both with id=(-currentid).
                   * All job nodes have a negative id. *)
                  with Not_found -> Hashtbl.add hashtnameid dest (-currentid); Hashtbl.add hashtidname (-currentid) dest;

                    (* Add an arc between the job node and the Destination node in the graph. *)
                    let graph3 = new_arc (ensure (ensure graph3 (-currentid)) (-1)) {src=(-currentid); tgt=(-1); lbl=1} in

                    (* Add an arc between the person node and the job node in the graph. *)
                    let graph3 = new_arc (ensure (ensure graph3 idpers) (-currentid)) {src=idpers; tgt=(-currentid); lbl=1} in
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
  {graph=final_graph; namefromid=(Hashtbl.find hashtidname)}
