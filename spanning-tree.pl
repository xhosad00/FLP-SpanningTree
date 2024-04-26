%  TASK : Kostra grafu - Spanning Tree
%  @file spanning-tree.pl
%  @author Adam Hos (xhosad00)
%  @brief Spanning Tree - program finds all possible Spanning Tree for a given graph (not directed)

/************Code from input2.pl******************/

read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),
		[C|LL] = L).

isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]).

split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).
/************Code from input2.pl******************/

/************list combination, taken from StackOverflow******************/
% link https://stackoverflow.com/questions/41662963/all-combinations-of-a-list-without-doubles-in-prolog - first answer
combs([],[]).
combs([H|T],[H|T2]) :-
    combs(T,T2).
combs([_|T],T2) :-
    combs(T,T2).
/************list combination, taken from StackOverflow******************/

/**
* getHead(+List : list, ?Head)
* succeeds if Head is the first element of the given list List
* 
* @param List input list
* @param Head head of the input list
*/
getHead([H|_], H).

/**
* sameLenLists(+List1 : list, +List2 : list)
* 
* succeeds if lists have the same length
*
* @param List1 first list
* @param List2 second list
*/
sameLenLists([], []).
sameLenLists([_|T1], [_|T2]) :-
    sameLenLists(T1, T2).


/**
* vertices(+Lists : list, -Vertices : list)
* 
* get all vertecies from input by joint all vertecies and using sort
* to keep only unique
*
* @param Lists list of lists - each list is one edge
* @param Vertices list of unique elements extracted from Lists
*/
vertices([[]], []).
vertices(S, Ver) :-
    flatten(S, Flat),
    sort(Flat, Ver). % changed list_to_set -> sort TODO untested

/**
* spanLen(+Vertices : list, ?Length : integer)
* 
* succeeds if Length is the length of the list Vertices - 1
*
* @param Vertices list of vertices
* @param Length length of the SpanningTree
*/
spanLen([], 0).
spanLen(Ver, Len) :-
    length(Ver, L),
    Len is L - 1.

/**
* formatEdge(+Edge : list, -FormattedEdge : list)
* 
* format the edge from input style to edge style
* (from list of lists to a list)
*
* @param Edge input edge represented as a list of lists
* @param FormattedEdge reformatted edge represented as a list
*/
formatEdge([[A],[B]], [A,B]).

/**
* edges(+Input : list, -Edges : list)
* 
* format each edge in Input using formatEdge
*
* @param Input list of lists representing edges
* @param Edges list containing all formatted edges
*/
edges([[]], []).
edges(Input, R) :-
    maplist(formatEdge, Input, R).

/**
* combsOfLength(+List : list, +Length : integer, -Comb : list)
* 
* gets all combinations of input lst tha is of length Len
*
* @param List input list
* @param Length length of combinations
* @param Comb list containing combinations of length Length
*/

combsOfLength(L, Len, Comb) :-
    combs(L, Comb),
    length(Comb, Len).

/**
* spanTreeVertices(+SpanningTree : list, -Vertices : list)
* 
* get a list of verticies that are in the SpanningTree
*
* @param SpanningTree (list of edges)
* @param Vertices list of unique vertices extracted from SpanningTree
*/
spanTreeVertices([], []).
spanTreeVertices(Span, Ver):-
    flatten(Span, Flat),
    list_to_set(Flat, Set),
    sort(Set, Ver).   


/**
* getNodeNeighbours(+Edges : list, +Node : any, +SeenVer : list, +SeenEdges : list, ?NextSeenVer : list, ?NextSeenEdges : list)
* 
* succeeds if NextSeenVer and NextSeenEdges are lists containing the updated versions of SeenVer and SeenEdges 
* go over the entire Edge list. If either vertex of current edge == Node, add this edge to seenEdges and the 
* other vertex of this Edge to seen vertecies
*
* @param Edges list of edges
* @param Node node/vertex for which neighbors are to be found
* @param SeenVer list of seen vertices
* @param SeenEdges list of seen edges
* @param NextSeenVer updated list of seen vertices
* @param NextSeenEdges updated list of seen edges
*/
getNodeNeighbours([], _, SeenVer, SeenEdges, SeenVer, SeenEdges).
getNodeNeighbours([Edge|T], Node, SeenVer, SeenEdges, NextSeenVer, NextSeenEdges):- % if the edge was allready seen, continue
    memberchk(Edge, SeenEdges),
    getNodeNeighbours(T, Node, SeenVer, SeenEdges, NextSeenVer, NextSeenEdges).
getNodeNeighbours([[A,B]|T], Node, SeenVer, SeenEdges, NextSeenVer, NextSeenEdges):-
    (   A == Node,
        \+ memberchk(B, SeenVer)
    ->
        append(SeenVer, [B], SeenVer2)
        ,append(SeenEdges, [[A,B]], SeenEdges2)
    ;   B == Node,
        \+ memberchk(A, SeenVer)
    ->
        append(SeenVer, [A], SeenVer2)
        ,append(SeenEdges, [[A,B]], SeenEdges2)
    ;   
        SeenVer2 = SeenVer
        ,SeenEdges2 = SeenEdges
    ),
    getNodeNeighbours(T, Node, SeenVer2, SeenEdges2, NextSeenVer, NextSeenEdges)
    % ,append(SeenVer2, NextSeenVer, RetSeenVer)
    .


/**
* getNodeGraphCycle(+Span : list, +Nodes : list, ?RetSeenVer : list, ?RetSeenEdges : list)
* 
* use BFS search to search the candidate spanning tree starting from Node
* picks first node in list and expands it (finds all its neighbours, only keeps yet undiscovered Vertecies)
* add newly sicvoered Vericies to Nodes, and continue until Span is empty or ther
*
* @param Span list of edges
* @param Nodes list of nodes
* @param RetSeenVer list of seen vertices
* @param RetSeenEdges list of seen edges
*/
getNodeGraphCycle([], _, [], []).
getNodeGraphCycle(Span, [Node|Nodes], RetSeenVer, RetSeenEdges):-
    getNodeNeighbours(Span, Node, [], [], SeenVer, SeenEdges),
    subtract(Span, SeenEdges, CutSpan),    
    subtract(SeenVer, Nodes, UnseenVer),
    append(Nodes, UnseenVer, NewNodes),
    (   length(NewNodes, 0)
    ->
        RetSeenVer = SeenVer,
        RetSeenEdges = SeenEdges
    ;
        getNodeGraphCycle(CutSpan, NewNodes, NextSeenVer, NextSeenEdges),
        append(SeenVer, NextSeenVer, RetSeenVer),
        append(SeenEdges, NextSeenEdges, RetSeenEdges)
    )
    .

/**
* getNodeGraph(+Span : list, +Node : any, -RetNodes : list, -FoundEdges : list)
* 
* succeeds if RetNodes and FoundEdges are lists containing the seen vertices and edges after going
* as far as possible from the starting node
* Idea: Starts BFS search through SpanningTree starting at Node. Finds all connected nodes and edges and unifies
* them in RetNodes and FoundEdges
*
* @param Span list of edges
* @param Node node for which neighbors are to be found
* @param RetNodes list of seen vertices
* @param FoundEdges list of seen edges
*/
getNodeGraph([], _, [], []).
getNodeGraph(_, [], [], []).
getNodeGraph(Span, Node, RetNodes, FoundEdges):-
    % getHead(Nodes, FirstNode),
    getNodeGraphCycle(Span, [Node], FoundNodes, FoundEdges ),
    append([Node], FoundNodes, UnsortedNodes),
    sort(UnsortedNodes, RetNodes)
    .

/**
* filterSolution(+Span : list, +VerCnt : integer)
* 
* succeeds if node graph starting from SpanningTree first Vertex has the same length as the SpanningTree
*
* @param Span list of edges representing a spanning tree
* @param VerCnt count of vertices in the spanning tree
*/
filterSolution(Span, VerCnt):-
    getHead(Span, [A,_]),
    getNodeGraph(Span, A, _, RetSeenEdges),
    spanTreeVertices(RetSeenEdges, SpanVer),
    length(SpanVer, VerCnt).


/**
* filterSolutions(+PossibleSolutions : list, ?Solutions : list, +VerCnt : integer)
* 
* succeeds if Solutions is a list containing valid spanning trees from PossibleSolutions
*
* @param PossibleSolutions list of possible spanning trees
* @param Solutions list of valid spanning trees
* @param VerCnt count of vertices in a correct spanning tree
*/
filterSolutions([], [], _).
filterSolutions([Span|T], Solutions, VerCnt):-
    ( filterSolution(Span, VerCnt)
    ->
        Solutions = [Span|Rest]
    ;
        Solutions = Rest
    ),
    filterSolutions(T, Rest, VerCnt).

/**
* printSolutionFormated(+Solution : list)
* 
* succeeds if the given Solution is printed in formatted output style
* example for [[1,2],[2,3]] where [1,2] and [2,3] are edges
* 1-2 2-3
*
* @param Solution list representing a spanning tree
*/
printSolutionFormated([]).
printSolutionFormated([[A,B]|T]):-
    write(A),
    write('-'),
    write(B),
    (   \+ T == []
    ->
        write(' ')
    ;
        true
    ),
    printSolutionFormated(T)
    .

/**
* printSolutionsFormated(+Solutions : list)
* 
* succeeds if each all Solutions are fromated in the proper output format
*
* @param Solutions list of spanning trees solutions
*/
printSolutionsFormated([]).
printSolutionsFormated([H|T]):-
    printSolutionFormated(H),
    nl,
    printSolutionsFormated(T).

/**
 * start/0 predicate that starts the program
 * loads input from console
 * creates possible solutions of spanning trees and then filters them accordingly
 * if there are any left, print the to output with formatted print style
 *
 */
start :-

    prompt(_, ''),
    read_lines(LL),
    split_lines(LL,Input),
% Input = [
% [['A'],['B']],
% [['A'],['C']],
% [['A'],['D']],
% [['B'],['C']]],
    edges(Input, Edges),
    vertices(Input, Ver),
    length(Ver, VerCnt),
    spanLen(Ver, SpanLen),
    findall(Comb, combsOfLength(Edges, SpanLen, Comb), PossibleSolutions),
    % writeln('Printing'),
    filterSolutions(PossibleSolutions, Solutions, VerCnt),
    % printListNL(Solutions),
    printSolutionsFormated(Solutions)
    % writeln('End')
    .



printListNL([]).
% Recursive case: print the head of the list and recurse on the tail
printListNL([Head|Tail]) :-
    writeln(Head),
    printListNL(Tail).