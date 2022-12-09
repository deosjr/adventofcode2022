:- ['../lib/io.pl'].

parse([]) --> eos.
parse([Dir/N|T]) --> nonblanks(D), white, integer(N), "\n", parse(T),
    {atom_codes(A, D), downcase_atom(A, Dir)}.

dir(u, 0, 1).
dir(l, -1, 0).
dir(d, 0, -1).
dir(r, 1, 0).

add(X/Y, DX/DY, NX/NY) :-
    NX #= X + DX,
    NY #= Y + DY.

step(Dir, Rope, NewRope) :-
    dir(Dir, X, Y),
    Rope = [Head|Tail],
    add(Head, X/Y, NewHead),
    foldl(move, Tail, NewTail, NewHead, _),
    NewRope = [NewHead|NewTail].

% Tail, NewTail, Head, NewTail
move(TX/TY, New, HX/HY, New) :-
    DX #= HX - TX,
    DY #= HY - TY,
    ( update(TX/TY, DX/DY, Update) ->
      New = Update ;
      New = TX/TY
    ).

update(TX/TY, DX/DY, TX/NY) :-
    DX #= 0, abs(DY) #> 1,
    NY #= TY + DY - (DY // abs(DY)).
update(TX/TY, DX/DY, NX/TY) :-
    DY #= 0, abs(DX) #> 1,
    NX #= TX + DX - (DX // abs(DX)).
update(TX/TY, DX/DY, NX/NY) :-
    DX #\= 0, DY #\= 0, abs(DX)+abs(DY) #> 2,
    NX #= TX + (DX // abs(DX)),
    NY #= TY + (DY // abs(DY)).

simulate(Input, Out1, Out9) :-
    length(Rope, 10),
    maplist(=(0/0), Rope),
    rb_empty(Root),
    rb_insert(Root, 0/0, true, Start),
    simulate(Input, Rope, Start, Out1, Start, Out9).

simulate([], _, X, X, Y, Y).

simulate([D/N|T], Rope, In1, Out1, In9, Out9) :-
    step(D, Rope, NewRope),
    nth0(1, NewRope, Pos1),
    nth0(9, NewRope, Pos9),
    rb_insert(In1, Pos1, true, New1),
    rb_insert(In9, Pos9, true, New9),
    ( N #= 1 ->
      Next = T ;
      NN #= N - 1, Next = [D/NN|T]
    ),
    simulate(Next, NewRope, New1, Out1, New9, Out9).

run :-
    input_stream(9, parse(In)),
    simulate(In, P1, P9),
    rb_size(P1, Ans1),
    write_part1(Ans1),
    rb_size(P9, Ans2),
    write_part2(Ans2).
