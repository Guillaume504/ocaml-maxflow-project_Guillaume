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
  let graph = bires.graph in 
  let graph = fordfulkerson graph 0 (-1) in
  let graph = trimgraph graph in
  let () = bi_write_file bires.namefromid "answer.txt" graph in

  export ~nodename:bires.namefromid ~labelform:(label_capacity bires.graph) outfile graph;

  ()
