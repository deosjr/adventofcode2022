:- ['../lib/io.pl'].

parse([]) --> eos.
parse([[S1,E1,S2,E2]|T]) --> parse_assignment(S1-E1), ",", parse_assignment(S2-E2), "\n", parse(T).
parse_assignment(Start-End) --> integer(Start), "-", integer(End).

contains([S1,E1,S2,E2]) :-
    S2 #>= S1,
    E2 #=< E1,
    E2 #>= S1.
contains([S1,E1,S2,E2]) :-
    S1 #>= S2,
    E1 #=< E2,
    E1 #>= S2.

disjoint([S1,E1,S2,E2]) :-
    E1 #< S2 #\/ E2 #< S1.

part1(List, Ans) :-
    include(contains, List, Filtered),
    length(Filtered, Ans).

part2(List, Ans) :-
    exclude(disjoint, List, Filtered),
    length(Filtered, Ans).

run :-
    input_stream(4, parse(List)),
    part1(List, Ans1),
    write_part1(Ans1),
    part2(List, Ans2),
    write_part2(Ans2).

% BONUS: testing the reverse:
% generating past include/exclude leads to interesting behaviour
% test_reverse1(X,1) only generates lists of length 1
% test_reverse2(X,1) diverges immediately (generates infinite Y=0 examples first)
test_reverse1(X, Y) :-
    part1(X, Y),
    maplist([A]>>(A ins 1..99), X),
    maplist(label, X).

test_reverse2(X, Y) :-
    part2(X, Y),
    maplist([A]>>(A ins 1..99), X),
    maplist(label, X).
