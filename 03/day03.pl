:- ['../lib/io.pl'].

parse([]) --> eos.
parse([S|X]) --> nonblanks(S), "\n", parse(X).

priority(Item, P) :-
    ( Item #> 90 ->
      P #= Item - 96 ;
      P #= Item - 38).

part1(List, Ans) :-
    part1(List, 0, Ans).

part1([], Acc, Acc).
part1([Rucksack|Rest], Acc, Ans) :-
    length(A, L),
    length(B, L),
    append(A, B, Rucksack),
    member(X, A),
    member(X, B),
    priority(X, N),
    AccN #= Acc + N,
    part1(Rest, AccN, Ans).

part2(List, Ans) :-
    part2(List, 0, Ans).

part2([], Acc, Acc).
part2([A,B,C|Rest], Acc, Ans) :-
    member(X, A),
    member(X, B),
    member(X, C),
    priority(X, N),
    AccN #= Acc + N,
    part2(Rest, AccN, Ans).

run :-
    input_stream(3, parse(List)),
    part1(List, Ans1),
    write_part1(Ans1),
    part2(List, Ans2),
    write_part2(Ans2).

% BONUS: testing the reverse:
rucksack(Rucksack) :-
    N in 1..10,
    N mod 2 #= 0,
    length(Rucksack, N),
    Rucksack ins 65..90\/97..122.

test_reverse1(X, Y) :-
    length(X, 4),
    maplist(rucksack, X),
    part1(X, Y),
    maplist(label, X).

test_reverse2(X, Y) :-
    length(X, 6),
    maplist(rucksack, X),
    part2(X, Y),
    maplist(label, X).
