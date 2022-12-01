:- ['../lib/io.pl'].

parse(Nums) --> integer(N), blanks, eos, {assertz(calories([N|Nums]))}.
parse(Nums) --> integer(N), "\n\n", parse([]), {assertz(calories([N|Nums]))}.
parse(Nums) --> integer(N), "\n", parse([N|Nums]).

run :-
    input_stream(1, parse([])),
    findall(X, calories(X), Input),
    maplist([X,Y]>>(sum(X, #=, Y)), Input, Sums),
    sort(Sums, Sorted),
    reverse(Sorted, Reverse),
    Reverse = [A,B,C|_],
    write_part1(A),
    N #= A+B+C,
    write_part2(N).
