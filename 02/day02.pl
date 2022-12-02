:- ['../lib/io.pl'].

% NOTE: X is opponent!
parse(0) --> eos.
parse(N) --> parse_atom(X), " ", parse_atom(Y), "\n", parse(NN),
    {round(X,Y,Score), N #= NN + Score}.

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

round(X, X, Score) :-
    score(X, N),
    Score #= N + 3.
round(X, Y, Score) :-
    X \= Y,
    score(Y, N),
    ( beats(Y, X) ->
      Score #= N + 6;
      Score #= N).

run :-
    input_stream(2, parse(X)),
    write_part1(X).
