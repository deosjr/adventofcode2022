:- ['../lib/io.pl'].

parse([]) --> eos.
parse([X/Y|T]) --> parse_assignment(X), ",", parse_assignment(Y), "\n", parse(T).
parse_assignment(Start-End) --> integer(Start), "-", integer(End).

contains((S1-E1)/(S2-E2)) :-
    S2 #>= S1,
    E2 #=< E1,
    E2 #>= S1.
contains((S1-E1)/(S2-E2)) :-
    S1 #>= S2,
    E1 #=< E2,
    E1 #>= S2.

disjoint((S1-E1)/(S2-E2)) :-
    E1 #< S2 #\/ E2 #< S1.

run :-
    input_stream(4, parse(List)),
    include(contains, List, Out1),
    length(Out1, Ans1),
    write_part1(Ans1),
    exclude(disjoint, List, Out2),
    length(Out2, Ans2),
    write_part2(Ans2).
