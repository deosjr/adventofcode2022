:- ['../lib/io.pl'].

:- dynamic find/2.

parse --> eos.
parse --> parse_monkey(M), ": ", integer(N), "\n", parse,
    {assertz(monkey(M, N))}.
parse --> parse_monkey(M), ": ", parse_monkey(A), " ", string_without(" ", O), " ", parse_monkey(B), "\n", parse,
    {atom_codes(Op, O), assertz((monkey(M, N) :- monkey(A,X), monkey(B,Y), op(Op,X,Y,N)))}.
parse_monkey(Name) --> string_without(": \n", S), {atom_codes(Name, S)}.

op(+,X,Y,Z) :- Z #= X + Y.
op(-,X,Y,Z) :- Z #= X - Y.
op(*,X,Y,Z) :- Z #= X * Y.
op(/,X,Y,Z) :- Z #= X // Y.

find(Node, X) :-
    Body = (monkey(A,_), monkey(B,_), op(Op,_,_,_)),
    ( Node = A, Other = B ; Node = B, Other = A ),
    clause(monkey(Parent, N), Body),
    monkey(Other, Y),
    ( Node = A ->
      op(Op, X, Y, N) ; 
      op(Op, Y, X, N)
    ),
    find(Parent, N).

run :-
    input_stream(21, parse),
    monkey(root, Ans1),
    write_part1(Ans1),
    clause(monkey(root, _), Body),
    Body = (monkey(A,_), monkey(B,_), _),
    asserta((find(A,X) :- monkey(B,X))),
    asserta((find(B,X) :- monkey(A,X))),
    find(humn, Ans2),
    label([Ans2]),
    write_part2(Ans2).
