:- ['../lib/io.pl'].

parse([]) --> eos.
parse([S|T]) --> string_without("\n", S), "\n", parse(T).

rotate(Grid, Rotate90) :-
    rotate(Grid, [], Rotate90).
rotate(Grid, Acc, Acc) :-
    maplist([X]>>length(X, 0), Grid).
rotate(Grid, Acc, Rotated) :-
    maplist([X,Y,Z]>>(X=[Y|Z]), Grid, Heads, Tails),
    rotate(Tails, [Heads|Acc], Rotated).

rotations(Trees, T90, T180, T270) :-
    rotate(Trees, T90),
    rotate(T90, T180),
    rotate(T180, T270).

% does a sweep in 1 dimension, left to right
visible([H|T], [x|Visible]) :-
    visible(H, T, Visible).

visible(_, [], []).
visible(N, [H|T], [_|Visible]) :-
    N #>= H,
    visible(N, T, Visible).
visible(N, [H|T], [x|Visible]) :-
    N #< H,
    visible(H, T, Visible).

% sweep in 4 dimensions and unify
part1(Trees, Ans) :-
    rotations(Trees, T90, T180, T270),
    % then map and unrotate output
    maplist(visible, Trees, Out),
    maplist(visible, T90, Out90),
    maplist(visible, T180, Out180),
    maplist(visible, T270, Out270),
    rotations(Out, Out90, Out180, Out270),
    maplist(sumvar, Out, Sums),
    sum(Sums, #=, Ans).

sumvar([], 0).
sumvar([H|T], N) :-
    ( var(H) ->
      sumvar(T, N) ;
      N1 #= N - 1, sumvar(T, N1)
    ).

% for part2 lets do what we couldve done in part1:
% still generate 4 rotations, but then traverse all 4 explicitly after
% instead of unifying them (because that way we also dont rely on var/1).
view(List, Visible) :-
    view([0,0,0,0,0,0,0,0,0,0], List, Visible).

view(_, [], []).
view(Counts, [H|T], [ViewCount|Visible]) :-
    nth0(H, Counts, ViewCount),
    update_counts(H, Counts, NewCounts),
    view(NewCounts, T, Visible).

update_counts(_, [], []).
update_counts(N, [_|T], [1|NT]) :-
    N #>= 0,
    NN #= N - 1,
    update_counts(NN, T, NT).
update_counts(N, [H|T], [NH|NT]) :-
    N #< 0,
    NH #= H + 1,
    update_counts(N, T, NT).

part2(Trees, Ans) :-
    rotations(Trees, T90, T180, T270),
    % then map and unrotate output
    maplist(view, Trees, Out1),
    maplist(view, T90, OutT),
    maplist(view, T180, OutR),
    maplist(view, T270, OutTR),
    rotate(OutT, Y), rotate(Y, Z), rotate(Z, Out2),
    rotate(OutR, X), rotate(X, Out3),
    rotate(OutTR, Out4),
    % now traverse all 4 grids and build up a fifth
    join(Out1, Out2, Out3, Out4, Scores),
    maplist(max, Scores, Max),
    max(Max, Ans).

join([],[],[],[],[]).
join([[]|T1], [[]|T2], [[]|T3], [[]|T4], [[]|T]) :-
    join(T1, T2, T3, T4, T).

join([[H1|T1]|TT1], [[H2|T2]|TT2], [[H3|T3]|TT3], [[H4|T4]|TT4], [[H|T]|TT]) :-
    H #= H1 * H2 * H3 * H4,
    join([T1|TT1], [T2|TT2], [T3|TT3], [T4|TT4], [T|TT]).

max([X], X).
max([X,Y|T], N) :-
    max([Y|T], M),
    ( X #> M -> N #= X ; N #= M ).

run :-
    input_stream(8, parse(In)),
    maplist((maplist([X,Y]>>(Y#=X-48))), In, Trees),
    part1(Trees, Ans1),
    write_part1(Ans1),
    part2(Trees, Ans2),
    write_part2(Ans2).

% BONUS: testing the reverse:
% part1 is impure due to var/1 but using unification is such a cool trick
% generates answers just fine for the generic case, but for Score=8
% it has to crunch to all the possible solutions for Score < 8 first
test_reverse(Trees, Score) :-
    Score #>= 0,
    length(Trees, 5),
    maplist([X]>>(length(X, 5), X ins 0..9), Trees),
    part2(Trees, Score).
