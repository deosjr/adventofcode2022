:- ['../lib/io.pl'].

parse(D, M) --> parse_drawing(D), parse_moves(M).

parse_drawing([]) --> string_without("\n", _), "\n\n".
parse_drawing([[C|H]|T]) --> parse_crate(C), " ", parse_drawing([H|T]).
parse_drawing([[C]|X]) --> parse_crate(C), "\n", parse_drawing(X).
parse_crate(empty) --> white, white, white.
parse_crate(C) --> "[", nonblank(C), "]".

parse_moves([]) --> eos.
parse_moves([N-From-To|X]) --> "move ", integer(N), " from ", integer(From), " to ", integer(To), "\n", parse_moves(X).

crane(Crates, [], _, Ans) :-
    maplist(nth0(0), Crates, Ans).

crane(Crates, [N-F-T|Moves], Over9000, Ans) :-
    nth1(F, Crates, From),
    nth1(T, Crates, To),
    length(Prefix, N),
    append(Prefix, Suffix, From),
    ( Over9000 ->
      Pref = Prefix ;
      reverse(Prefix, Pref)
    ),
    append(Pref, To, NewTo),
    select(From, Crates, Suffix, C),
    select(To, C, NewTo, NewCrates),
    nth1(F, NewCrates, Suffix),
    nth1(T, NewCrates, NewTo),
    crane(NewCrates, Moves, Over9000, Ans).

run :-
    input_stream(5, parse(Drawing, Moves)),
    transpose(Drawing, Transposed),
    maplist(exclude(=(empty)), Transposed, Crates),
    crane(Crates, Moves, false, Atoms1),
    atom_codes(Ans1, Atoms1),
    write_part1(Ans1),
    crane(Crates, Moves, true, Atoms2),
    atom_codes(Ans2, Atoms2),
    write_part2(Ans2).

% BONUS: testing the reverse:
% use_test_input.
test_reverse(Moves) :-
    input_stream(5, parse(Drawing, _)),
    transpose(Drawing, Transposed),
    maplist(exclude(=(empty)), Transposed, Crates),
    string_codes("CMZ", Codes),
    length(Moves, 4),
    maplist([X]>>(X=N-F-T, [N,F,T] ins 1..3, label([N,F,T])), Moves),
    crane(Crates, Moves, false, Codes).
