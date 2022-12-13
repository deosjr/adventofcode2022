:- ['../lib/io.pl'].

:- dynamic set/2.

parse(_,_,_,_) --> eos.
parse(X,Y,Start,End) --> nonblank(83),
    {Start=X-Y, NX #= X+1, assertz(pos(X-Y,97))},
    parse(NX,Y,Start,End).
parse(X,Y,Start,End) --> nonblank(69),
    {End=X-Y, NX #= X+1, assertz(pos(X-Y,122))},
    parse(NX,Y,Start,End).
parse(X,Y,Start,End) --> nonblank(C),
    {C \= 83, C \= 69, assertz(pos(X-Y,C)), NX #= X+1},
    parse(NX,Y,Start,End).
parse(_,Y,Start,End) --> blank,
    {NY #= Y+1}, parse(0,NY,Start,End).

% breadth-first search floodfill
floodfill([], [], _).
floodfill([], [H|T], PathLen) :-
    NewPathLen #= PathLen + 1,
    floodfill([H|T], [], NewPathLen).
floodfill([Coord|T], New, PathLen) :-
    pos(Coord, C),
    ( not(set(Coord,_)) ->
      ( 
        assertz(set(Coord, PathLen)),
        neighbours(Coord, C, Neighbours),
        append(New, Neighbours, NNew),
        floodfill(T, NNew, PathLen)
      ) ;
      floodfill(T, New, PathLen)
    ).

neighbours(X-Y, C, Neighbours) :-
    Xmin #= X-1, Xplus #= X+1, Ymin #= Y-1, Yplus #= Y+1,
    List = [Xmin-Y, Xplus-Y, X-Ymin, X-Yplus],
    include(nfilter(C), List, Neighbours).

nfilter(Last, Coord) :-
    pos(Coord, C),
    not(set(Coord,_)),
    C+1 #>= Last.

% NOTE: tried to unify in a single rb_tree, that was way too slow.
% current version could be two rb_trees I guess but asserts work for now
run :-
    input_stream(12, parse(0,0,Start,End)),
    floodfill([End], [], 0),
    set(Start, Ans1),
    write_part1(Ans1),
    findall(X, (pos(C,97), set(C,X)), Ans),
    select(Ans2, Ans, Rem),
    maplist(#<(Ans2), Rem),
    write_part2(Ans2).
