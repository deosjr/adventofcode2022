:- ['../lib/io.pl'].

% NOTE: X is opponent!
parse([]) --> eos.
parse([X-Y|List]) --> parse_atom(X), " ", parse_atom(Y), "\n", parse(List).

parse_atom(A) --> string(S), {atom_string(UA,S), downcase_atom(UA,A)}.

lookup_atom(a, rock).
lookup_atom(x, rock).
lookup_atom(b, paper).
lookup_atom(y, paper).
lookup_atom(c, scissors).
lookup_atom(z, scissors).

beats(rock, scissors).
beats(paper, rock).
beats(scissors, paper).

score(rock, 1).
score(paper, 2).
score(scissors, 3).

game1(Rounds, Score) :-
    maplist([X,Y]>>(X=A-B, lookup_atom(A, C), lookup_atom(B,D), Y=C-D), Rounds, Mapped),
    maplist([X,Y]>>(X=A-B, game1(A,B,Y)), Mapped, Scores),
    sum(Scores, #=, Score).

game1(X, X, Score) :-
    score(X, N),
    Score #= N + 3.
game1(X, Y, Score) :-
    X \= Y,
    score(Y, N),
    ( beats(Y, X) ->
      Score #= N + 6;
      Score #= N).

game2(Rounds, Score) :-
    maplist([X,Y]>>(X=A-B, lookup_atom(A, C), Y=C-B), Rounds, Mapped),
    maplist([X,Y]>>(X=A-B, game2(A,B,Y)), Mapped, Scores),
    sum(Scores, #=, Score).

game2(X, x, Score) :-
    beats(X, Y),
    game1(X, Y, Score).
game2(X, y, Score) :-
    game1(X, X, Score).
game2(X, z, Score) :-
    beats(Y, X),
    game1(X, Y, Score).

run :-
    input_stream(2, parse(List)),
    game1(List, Ans1),
    write_part1(Ans1),
    game2(List, Ans2),
    write_part2(Ans2).

% BONUS: testing the reverse:
test_reverse1(X,Y) :-
    length(X, 4),
    game1(X, Y).

test_reverse2(X,Y) :-
    length(X, 4),
    game2(X, Y).
