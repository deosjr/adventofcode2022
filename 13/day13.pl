:- ['../lib/io.pl'].

parse([]) --> "\n", eos.
parse(X) --> "\n\n", parse(X).
parse([Left-Right|T]) --> string_without("\n", L), "\n", string_without("\n", R), {
    atom_codes(AL, L),
    read_term_from_atom(AL, Left, []),
    atom_codes(AR, R),
    read_term_from_atom(AR, Right, [])
    }, parse(T).

% reifying the comparison like clpfd:zcompare does
ordered(=, [], []).
ordered(<, [], [_|_]).
ordered(>, [_|_], []).
ordered(C, [HL|TL], [HR|TR]) :-
    ordered(CC, HL, HR),
    ( CC = (=) ->
      ordered(C, TL, TR) ;
      C = CC ).
ordered(C, Left, Right) :-
    integer(Left),
    is_list(Right),
    ordered(C, [Left], Right).
ordered(C, Left, Right) :-
    is_list(Left),
    integer(Right),
    ordered(C, Left, [Right]).
ordered(C, Left, Right) :-
    integer(Left),
    integer(Right),
    zcompare(C, Left, Right).

run :-
    input_stream(13, parse(Pairs)),
    enumerate(Pairs, Enum),
    include([N-(X-Y)]>>ordered(<,X,Y), Enum, Ordered),
    maplist([N-_,M]>>(M#=N+1), Ordered, Indices),
    sum(Indices, #=, Ans1),
    write_part1(Ans1),
    Div1 = [[2]],
    Div2 = [[6]],
    foldl([A,B,C]>>(A=X-Y, append([X,Y],B,C)), Pairs, [Div1,Div2], Packets),
    predsort(ordered, Packets, Sorted),
    nth1(D1, Sorted, Div1),
    nth1(D2, Sorted, Div2),
    Ans2 #= D1 * D2,
    write_part2(Ans2).
