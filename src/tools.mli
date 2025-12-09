open Graph

(* Duplicate a graph without its arcs. *)
val clone_nodes: 'a graph -> 'b graph

(* Map all arcs of a graph by a function. *)
val gmap: 'a graph -> ('a -> 'b) -> 'b graph

(* Add a number to the value of the arc between 2 nodes.
 * If the arc does not exist, it is created *)
val add_arc: int graph -> id -> id -> int -> int graph