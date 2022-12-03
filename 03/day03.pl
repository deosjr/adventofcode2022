:- ['../lib/io.pl'].

parse([]) --> eos.
parse([S|X]) --> nonblanks(S), "\n", parse(X).

priority(Item, P) :-
    ( Item #> 90 ->
      P #= Item - 96 ;
      P #= Item - 38).

part1(Rucksack, N) :-
    length(A, L),
    length(B, L),
    append(A, B, Rucksack),
    member(X, A),
    memberchk(X, B),
    priority(X, N).

part2(List, Ans) :-
    part2(List, 0, Ans).

part2([], Acc, Acc).
part2([A,B,C|Rest], Acc, Ans) :-
    member(X, A),
    memberchk(X, B),
    memberchk(X, C),
    priority(X, N),
    AccN #= Acc + N,
    part2(Rest, AccN, Ans).

run :-
    input_stream(3, parse(List)),
    maplist(part1, List, Mapped),
    sum(Mapped, #=, N),
    write_part1(N),
    part2(List, Ans),
    write_part2(Ans).
