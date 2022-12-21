:- ['../lib/io.pl'].

parse --> eos.
parse --> parse_monkey(M), ": ", integer(N), "\n", parse,
    {assertz(monkey(M, N))}.
parse --> parse_monkey(M), ": ", parse_monkey(A), " ", string_without(" ", O), " ", parse_monkey(B), "\n", parse,
    {atom_codes(Op, O), assertz((monkey(M, N) :- monkey(A,X), monkey(B,Y), op(Op,X,Y,N)))}.
parse_monkey(Name) --> string_without(": \n", S), {atom_codes(Name, S)}.

op(+,X,Y,Z) :-
    Z #= X + Y.
op(-,X,Y,Z) :-
    Z #= X - Y.
op(*,X,Y,Z) :-
    Z #= X * Y.
op(/,X,Y,Z) :-
    Z #= X // Y.

run :-
    input_stream(21, parse),
    monkey(root, Ans1),
    write_part1(Ans1),
    % TODO: root: pgnv + wcnp
    retractall(monkey(root, _)),
    assertz((monkey(root, X) :- monkey(pgnv,X), monkey(wcnp,X))),
    retractall(monkey(humn, _)),
    % TODO: write X instead of manual binary search...
    assertz((monkey(humn, X) :- X in 3876027195000..3876027200000, label([X]), writeln(X))),
    monkey(root, Ans2),
    write_part2(Ans2).
