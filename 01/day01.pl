:- ['../lib/io.pl'].

parse(Nums) --> integer(N), blanks, eos, {assertz(calories([N|Nums]))}.
parse(Nums) --> integer(N), "\n\n", parse([]), {assertz(calories([N|Nums]))}.
parse(Nums) --> integer(N), "\n", parse([N|Nums]).

top_three(List, A, B, C) :-
    mapsum(List, Sums),
    sort(Sums, Sorted),
    %permutation(Sums, Sorted),
    %chain(Sorted, #>=),
    reverse(Sorted, Reverse),
    Reverse = [A,B,C|_].

% maplist([X,Y]>>(sum(X, #=, Y)), List, Sums), but anon funcs like that are slow
mapsum([], []).
mapsum([List|T], [Sum|Sums]) :-
    sum(List, #=, Sum),
    mapsum(T, Sums).

% the problem with running this backwards is the sorting in top_three/4
% using permutation + checking sorted property is wayyy too slow
test_reverse(X) :-
    length(X, 3),
    maplist([L]>>(length(L,N), N in 1..3, L ins 1..3), X),
    top_three(X, 1, 2, 3),
    maplist(label, X).

% impure
run :-
    input_stream(1, parse([])),
    findall(X, calories(X), Input),
    top_three(Input, A, B, C),
    write_part1(A),
    N #= A+B+C,
    write_part2(N).
