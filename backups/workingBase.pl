%  TEMA : Kostra grafu
%  @file spanning-tree.pl
%  @author Adam Hos (xhosad00)
%  @brief Kostra grafu - program schopny nalezt vsechny kostry zadaneho neorientovaneho grafu



/************KÓD Z input2.pl******************/

% /** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).

% /** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

% /** rozdeli radek na podseznamy */
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1

% /** vstupem je seznam radku (kazdy radek je seznam znaku) */
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).
/************KÓD Z input2.pl******************/

/************kombinace listu, prevzato z StackOverflow******************/
% odkaz https://stackoverflow.com/questions/41662963/all-combinations-of-a-list-without-doubles-in-prolog - první odpověď
combs([],[]).
combs([H|T],[H|T2]) :-
    combs(T,T2).
combs([_|T],T2) :-
    combs(T,T2).
/************kombinace listu, prevzato z StackOverflow******************/

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

filterSolution(Span, VerCnt):-
    getHead(Span, [A,B]),
    getNodeGraph(Span, A, RetSeenVer, RetSeenEdges),
    spanTreeVertices(Span, SpanVer),
    % writeln(SpanVer),
    length(SpanVer, VerCnt).

filterSolutions([], [], _).
filterSolutions([Span|T], Solutions, VerCnt):-
    ( filterSolution(Span, VerCnt)
    ->
        Solutions = [Span|Rest]
    ;
        Solutions = Rest
    ),
    filterSolutions(T, Rest, VerCnt).

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

printSolutionsFormated([]).
printSolutionsFormated([H|T]):-
    printSolutionFormated(H),
    nl,
    printSolutionsFormated(T).


start :-
Input = [
[['A'],['B']],
[['A'],['C']],
[['A'],['D']],
[['B'],['C']],
[['C'],['D']]],
    edges(Input, Edges),
    vertices(Input, Ver),
    length(Ver, VerCnt),
    spanLen(Ver, SpanLen),
    findall(Comb, combsOfLength(Edges, SpanLen, Comb), PossibleSolutions),
    writeln('Printing'),
    filterSolutions(PossibleSolutions, Solutions, VerCnt),
    printSolutionsFormated(Solutions),
    writeln('End')
    .

