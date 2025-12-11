Base project for Ocaml project on Ford-Fulkerson. This project contains some simple configuration files to facilitate editing Ocaml in VSCode.

To use, you should install the *OCaml Platform* extension in VSCode.
Then open VSCode in the root directory of this repository (command line: `code path/to/ocaml-maxflow-project`).

Features :
 - full compilation as VSCode build task (Ctrl+Shift+b)
 - highlights of compilation errors as you type
 - code completion
 - view of variable types


A [`Makefile`](Makefile) provides some useful commands:

 - `make build` to compile. This creates an `ftest.exe` executable
 - `make format` to indent the entire project
 - `make edit` to open the project in VSCode
 - `make clean` to remove build artifacts

In case of trouble with the VSCode extension (e.g. the project does not build, there are strange mistakes), a common workaround is to (1) close vscode, (2) `make clean`, (3) `make build` and (4) reopen vscode (`make edit`).

To test the Ford-Fulkerson algorithm with graph "graphs/graph1.txt" between node 0 and 5
./testfordfulk.exe graphs/graph1.txt 0 5 "test1.txt" && dot -Tsvg test1.txt > test1.svg

To test the bipartite matching with context "graphs/bipartite.txt"
./testbipart.exe graphs/bipartite.txt "test2.txt" && dot -Tsvg test2.txt > test2.svg

To test the Busacker-Gowen algorithm with graph "graphs/maxflowmincost.txt" between node 0 and 5
./testbusago.exe graphs/maxflowmincost.txt 0 5 "test3.txt" && dot -Tsvg test3.txt > test3.svg