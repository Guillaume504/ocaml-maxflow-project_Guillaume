open Graph

type path = string

type 'a dijk = 
  { value: 'a ;
    cost: int }

val busackergowen: capacity dijk graph -> id -> id -> flow dijk graph

val bg_from_file: path -> capacity graph

val bg_write_file: (id -> string) -> path -> flow graph -> unit