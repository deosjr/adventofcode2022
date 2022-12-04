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
% only complete once maplist moved _before_ part1/2 check!
assignment(X) :-
    length(X, 4),
    X ins 1..3.

test_reverse1(X, Y) :-
    maplist(assignment, X),
    maplist(label, X),
    part1(X, Y).

test_reverse2(X, Y) :-
    maplist(assignment, X),
    maplist(label, X),
    part2(X, Y).
