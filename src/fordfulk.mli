open Graph

(* Apply the Ford-Fulkerson algorithm to the capacity graph and compute the result flow graph. *)
val fordfulkerson: int graph -> id -> id -> int graph

(* Apply the Ford-Fulkerson algorithm to a problem of bipartite matching. *)
val bipartitematching: int graph -> int graph