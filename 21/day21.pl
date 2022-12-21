:- ['../lib/io.pl'].

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

run :-
    input_stream(21, parse),
    monkey(root, Ans1),
    write_part1(Ans1),
    clause(monkey(root, _), Body),
    Body = (monkey(A,_), monkey(B,_), _),
    retractall(monkey(root, _)),
    assertz((monkey(root, X) :- monkey(A,X), monkey(B,X))),
    retractall(monkey(humn, _)),
    % TODO: write X instead of manual binary search...
    X in 3876027195000..3876027200000,
    label([X]),
    assertz(monkey(humn, X)),
    ( monkey(root, _) -> true ;
      retractall(monkey(humn, _)), fail ),
    write_part2(X).
