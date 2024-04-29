
# Spanning Tree

## Description

VUT FIT 2024 FLP logical project

## File
`spanning-tree.pl`
## Author
- Adam Hos (xhosad00)
##  Implementation
The program reads from standard input and prints to standard output. It parsers the input as a series of edges. Next it creates a list of all Edges and Vertecies and calculates the length of a spanning tree(SpanLen) for this graph (|Verticies| - 1). It creates all possible edge combinations (Spanning Tree possible solutions) with EdgeCount eaqual to SpanLen. Then it starts filtering by picking the first vertex in candidate spanning tree (can be any) and tries to find all connected vertecies using BFS. If the count of found vertices is eaqual to SpanLen, then this spanning tree is valid. At the end print all spanning trees with formated output stile and halt.

## Usage
Compile using makefile or the command 
swipl -q -g start -o flp23-log -c spanning-Tree.pl
and launch the file like so
./flp23-log < /path_to_input/input_file.txt > /path_to_output/output_file.txt  

Input should have the following format (in file in.txt:):
A B\n
A C\n
A D\n
B C\n
C D
Where each word is a vertex and two vertecies in a row represent a grapgh edge.

Example:
flp23-log.exe < in.txt
where the output is:
A-B A-C A-D
A-B A-C C-D
A-B A-D B-C
A-B A-D C-D
A-B B-C C-D
A-C A-D B-C
A-C B-C C-D
A-D B-C C-D