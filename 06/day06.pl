:- ['../lib/io.pl'].

parse(S) --> string_without("\n", S), "\n", eos.

uniq(List, N, Ans) :-
    uniq(List, N, N, Ans).
uniq(List, N, Acc, Ans) :-
    length(P, N),
    prefix(P, List),
    ( all_distinct(P) ->
      Acc #= Ans ;
      List = [_|T], Acc1 #= Acc + 1, uniq(T, N, Acc1, Ans)
    ).

run :-
    input_stream(6, parse(S)),
    S ins 97..122,
    uniq(S, 4, Ans1),
    write_part1(Ans1),
    uniq(S, 14, Ans2),
    write_part2(Ans2).
