(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

type path = string

(* Values are read as int. *)
val bg_from_file: path -> int dijk graph

(* Similarly, we write only an int dijk graph. *)
val bg_write_file: path -> int dijk graph -> unit

(* Export the int dijk graph into a svg .txt file.
 * nodename can be entered to change the name of the nodes in the svp graph.
 * labelform can be entered to change the form of the label in the svg graph. *)
val bg_export: ?nodename:(id -> path) -> ?labelform:(int dijk arc -> string) -> path -> int dijk graph -> unit

(* Compute the flow of an arc into a formated string "flow/capacity" given its capacity graph *)
val bg_label_capacity: int dijk graph -> int dijk arc -> string