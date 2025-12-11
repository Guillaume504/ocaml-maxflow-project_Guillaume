open Graph

type path = string

(* Apply the Ford-Fulkerson algorithm to the capacity dijk graph and compute the result flow dijk graph.
 * The algorithm is based on a Dijkstra algorithm in order to find the lowest cost path. *)
val busackergowen: int dijk graph -> id -> id -> int dijk graph