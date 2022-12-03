:- ['../lib/io.pl'].

% using assert instead of parsing into list, because order doesnt matter
parse(Nums) --> integer(N), blanks, eos, {assertz(calories([N|Nums]))}.
parse(Nums) --> integer(N), "\n\n", parse([]), {assertz(calories([N|Nums]))}.
parse(Nums) --> integer(N), "\n", parse([N|Nums]).

select_max(X, List, Rem) :-
    select(X, List, Rem),
    maplist(#>(X), Rem).

top_three(List, A, B, C) :-
    mapsum(List, Sums),
    select_max(A, Sums, Rem1),
    select_max(B, Rem1, Rem2),
    select_max(C, Rem2, _).

% maplist([X,Y]>>(sum(X, #=, Y)), List, Sums), but anon funcs like that are slow
mapsum([], []).
mapsum([List|T], [Sum|Sums]) :-
    sum(List, #=, Sum),
    mapsum(T, Sums).

% impure
run :-
    input_stream(1, parse([])),
    findall(X, calories(X), Input),
    top_three(Input, A, B, C),
    write_part1(A),
    N #= A+B+C,
    write_part2(N).

% BONUS: testing the reverse
simple_constraints(List) :-
    length(List, N),
    N in 1..3,
    List ins 1..3.

test_reverse(X) :-
    length(X, 3),
    maplist(simple_constraints, X),
    top_three(X, 3, 2, 1),
    maplist(label, X).
