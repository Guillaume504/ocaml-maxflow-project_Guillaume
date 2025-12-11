open Gfile
open Fordfulk
open Bfile
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) outfile(2) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)
  
  (* These command-line arguments are not used for the moment. *)
  in

  (* Open file *)
  let bires = bi_from_file infile in

  (* Take the graph from the bipartres *)
  let graph = bires.graph in 

  (* Apply the bipartite matching algorithm *)
  let graph = bipartitematching graph in

  (* Write the resulted matching in file "answer.txt" *)
  let () = bi_write_file bires.namefromid "answer.txt" graph in

  (* Export the resulted flow graph into a dot file "outfile" *)
  export ~nodename:bires.namefromid ~labelform:(label_capacity bires.graph) outfile graph;
  ()
