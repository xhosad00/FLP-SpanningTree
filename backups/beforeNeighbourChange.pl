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


getHead([H|_], H).
getTail([_|T], T).

vertices([[]], []).
vertices(S, Ver) :-
    flatten(S, Flat),
    sort(Flat, Ver). % changed list_to_set -> sort TODO untested

splineLen([], 0).
splineLen(Ver, Len) :-
    length(Ver, L),
    Len is L - 1.

formatEdge([[A],[B]], [A,B]).

edges([[]], []).
edges(Input, R) :-
    maplist(formatEdge, Input, R).

combsOfLength(L, Len, Comb) :-
    combs(L, Comb),
    length(Comb, Len).

printListNL([]).
% Recursive case: print the head of the list and recurse on the tail
printListNL([Head|Tail]) :-
    writeln(Head),
    printListNL(Tail).

splineVertices([], []).
splineVertices(Spline, Ver):-
    flatten(Spline, Flat),
    list_to_set(Flat, Set),
    sort(Set, Ver).
    

filterByPath(_, Ver, Ver, true).
filterByPath([], _, _, false).
% filterByPath([[A,B]|T], Ver, Seen, Ret) :-

filterSolutions([], _ , []).
filterSolutions(_, [] , []).
% filterSolutions(PossibleSolutions, Ver, Solutions)
% filterSolutions([Span|T], Ver, [Solution|OtherSolutions]) :-

addNodeNeighbours([], _, SeenVer, SeenEdges, SeenVer, SeenEdges).
addNodeNeighbours([Edge|T], Node, SeenVer, SeenEdges, NextSeenVer, NextSeenEdges):- % pokud uz videl hranu, preskoc a pokracuj
    memberchk(Edge, SeenEdges),
    addNodeNeighbours(T, Node, SeenVer, SeenEdges, NextSeenVer, NextSeenEdges).
addNodeNeighbours([[A,B]|T], Node, SeenVer, SeenEdges, RetSeenVer, NextSeenEdges):-
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
    addNodeNeighbours(T, Node, SeenVer2, SeenEdges2, NextSeenVer, NextSeenEdges),
    append(SeenVer2, NextSeenVer, RetSeenVer)
    .
%TODO rework so that the seen edges are removet from T, then continue
joinAllNodesCycle([], _,  SeenVer, SeenEdges, SeenVer, SeenEdges). % ukonceni cyklu
joinAllNodesCycle([[A,B]|T], CheckedVer, SeenVer, SeenEdges, RetSeenVer, RetSeenEdges):- %TODO remove retSeenEdges
    writeln('Join'),
    writeln([[A,B]|T]),
    writeln([A,B]),
    append(SeenEdges, [[A,B]], SeenEdges1),
    (   \+ memberchk(A, CheckedVer)
    ->
        % addNodeNeighbours(T, A, SeenVer, SeenEdges, SeenVer2, SeenEdges2)
        addNodeNeighbours(T, A, CheckedVer, SeenEdges1, SeenVer2, SeenEdges2)
        % ,writeln('adding Nei')
        ,subtract(T, SeenEdges2, UnseenEdges),
        % writeln(UnseenEdges),
        % ,writeln([A,B])
        % ,writeln(T)
        % ,writeln(RetSeenVer)
        % ,writeln(RetSeenEdges)
        append(CheckedVer, [A], CheckedVer2)
    ;
        SeenEdges2 = SeenEdges1,
        CheckedVer2 = CheckedVer,
        UnseenEdges = T
        
    ),
    writeln(SeenVer2),
    writeln(SeenEdges2),
    (   \+ memberchk(B, CheckedVer)
    ->
        % addNodeNeighbours(T, A, SeenVer, SeenEdges, SeenVer2, SeenEdges2)
        addNodeNeighbours(UnseenEdges, B, CheckedVer2, SeenEdges2, SeenVer3, SeenEdges3),
        append(CheckedVer2, [B], CheckedVer3),
        subtract(UnseenEdges, SeenEdges3, UnseenEdges2)
    ;
        SeenVer3 = SeenVer2
        ,SeenEdges3 = SeenEdges2,
        CheckedVer3 = CheckedVer2,
        UnseenEdges2 = UnseenEdges
    ),
    writeln(SeenEdges3),
    % writeln(UnseenEdges2),
    nl,
    joinAllNodesCycle(UnseenEdges2, CheckedVer3, CheckedVer3, SeenEdges3, RetSeenVer, RetSeenEdges)
    % ,
    % writeln(SeenEdges3),
    % writeln(SeenEdges4),
    % append(SeenEdges3, SeenEdges4, RetSeenEdges)
    
    % RetSeenVer = CheckedVer3,
    % RetSeenEdges = SeenEdges3
    .

joinAllNodes([], [], []).
joinAllNodes([[A,B]|T], RetSeenVer, RetSeenEdges):-
    % SeenVer = [A], 
    % SeenEdges = [[A,B]],
    SeenVer = [], 
    SeenEdges = [],
    CheckedVer = [],
    joinAllNodesCycle([[A,B]|T], CheckedVer, SeenVer, SeenEdges, RetSeenVerUnsorted, RetSeenEdges),
    sort(RetSeenVerUnsorted, RetSeenVer).

start :-
    % prompt(_, ''),
    % read_lines(LL),
    % split_lines(LL,S),
%     Input = [
% [[1],[2]],
% [[1],[3]],
% [[2],[3]],
% [[2],[4]],
% [[4],[5]]
% ],
    % edges(Input, Edges),
    % vertices(Input, Ver),
    % splineLen(Ver, SplineLen),
    % writeln(Ver),
    % findall(Comb, combsOfLength(Edges, SplineLen, Comb), PossibleSolutions),
    % printListNL(PossibleSolutions),
    nl,
    nl,
    % filterSolutions(PossibleSolutions, Ver, Solutions),
    % writeln(Solutions),
    % printListNL(Solutions)
    % splineVertices([[1,2],[1,3],[2,3],[2,4]], R),
    
    Span = [[2,1],[2,3],[1,5],[2,4]],
    % Span = [[2,1],[2,3],[4,3],[2,5]],
    % filterByPath(SpanHeadless, [1,2,3], [], R),
    joinAllNodes(Span, R, E),
    writeln('Printing'),
    writeln(R),
    writeln(E)
    .
    % halt.



    % filterSolutions([Span|T], Ver, [Solution|OtherSolutions]):- % filtrace podle poctu vrcholu
    % filterSolutions(T, Ver, OtherSolutions),
    % splineVertices(Span, SpanVer),
    % nl,
    % (Ver == SpanVer ->
    %     Solution = Span
    % ;   
    %     Solution = []
    % ).

% reconsult('spanning-tree.pl').