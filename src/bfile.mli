open Graph

type path = string

(* Record type containing the capacity graph read from the bipartite txt file (all arcs have a capacity of 1)
 * and a fonction to get the name of a node from its id. *)
type bipartres = 
  { graph: capacity graph ;
    namefromid: (id -> string) }

(* Write down the result of the bipartite matching in the designated file. *)
val bi_write_file: (id -> string) -> path -> flow graph -> unit

(* Read the bipartite file and computes the associated bipartres. *)
val bi_from_file: path -> bipartres

(* Remove the origin and destination nodes from the given graph.
 * The origin must have the id '0' and the destination the id '-1' *)
val trimgraph: flow graph -> flow graph