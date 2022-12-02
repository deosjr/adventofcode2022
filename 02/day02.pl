:- ['../lib/io.pl'].

% NOTE: X is opponent!
parse(0, 0) --> eos.
parse(N1, N2) --> parse_atom(X), " ", parse_atom(Y), "\n", parse(NN1, NN2),
    {round1(X,Y,S1), round2(X,Y,S2), N1 #= NN1 + S1, N2 #= NN2 + S2}.

parse_atom(A) --> string(S), {atom_string(UA,S), downcase_atom(UA,LA), lookup_atom(LA,A)}.

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

round1(X, X, Score) :-
    score(X, N),
    Score #= N + 3.
round1(X, Y, Score) :-
    X \= Y,
    score(Y, N),
    ( beats(Y, X) ->
      Score #= N + 6;
      Score #= N).

% rock means lose
% paper means draw
% scissors means win
round2(X, rock, Score) :-
    beats(X, Y),
    round1(X, Y, Score).
round2(X, paper, Score) :-
    round1(X, X, Score).
round2(X, scissors, Score) :-
    beats(Y, X),
    round1(X, Y, Score).

run :-
    input_stream(2, parse(X, Y)),
    write_part1(X),
    write_part2(Y).
