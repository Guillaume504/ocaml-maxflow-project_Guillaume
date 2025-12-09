
open Graph

type path = string

type bipartres = 
   { graph: id graph ;
     namefromid: (id -> string) }

val bi_write_file: (id -> string) -> path -> id graph -> unit

val bi_from_file: path -> bipartres

val trimgraph: id graph -> id graph