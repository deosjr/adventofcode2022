:- ['../lib/io.pl'].

parse([]) --> eos.
parse([S-B|T]) -->
    "Sensor at ", parse_coord(S), ": closest beacon is at ", parse_coord(B), "\n", parse(T).
parse_coord(X/Y) -->
    "x=", integer(X), ", y=", integer(Y).

manhattan(PX/PY, QX/QY, D) :-
    D #= abs(PX - QX) + abs(PY - QY).

% does this sensor block any beacons on line Y, and in which X-range
blockline(Y, (SX/SY)-Beacon, MinX/MaxX) :-
    manhattan(SX/SY, Beacon, D),
    DDY #= abs(SY - Y),
    D #>= DDY,
    DDX #= D - DDY,
    MinX #= SX - DDX, MaxX #= SX + DDX.

simplify_range(PX/PY, QX/QY, PX/QY) :-
    PX #< QX, QX #=< PY, PY #=< QY.
simplify_range(PX/PY, QX/QY, PX/PY) :-
    QX #> PX, QX #< PY, QY #> PX, QY #< PY.

simplify([], []).
simplify([H|T], Simpler) :-
    select(R, T, Rem),
    ( simplify_range(H, R, N) ; simplify_range(R, H, N) ),
    % !, generates same answer multiple times otherwise
    simplify([N|Rem], Simpler).
simplify([H|T], [H|Simpler]) :-
    simplify(T, Simpler).

% each sensor has four edge lines ax+b just beyond its coverage
edges(S-B, Edges) :-
    S = _X/SY,
    blockline(SY, S-B, MinX/MaxX),
    LX #= MinX - 1, % (LX/SY) on edges
    RX #= MaxX + 1, % (RX/SY) on edges
    % rewrite A * LX + B = SY for A = 1 and A = -1
    LB1 #= SY + LX,
    LB2 #= SY - LX,
    RB1 #= SY + RX,
    RB2 #= SY - RX,
    Edges = [1/LB2, 1/RB2, -1/LB1, -1/RB1].

check(C, Sensor-Beacon) :-
    manhattan(Sensor, Beacon, D),
    manhattan(Sensor, C, D2),
    D2 #> D.

run :-
    input_stream(15, parse(Input)),
    convlist(blockline(2000000), Input, Ranges),
    simplify(Ranges, [SX/SY]),
    Ans1 #= abs(SX) + abs(SY),
    write_part1(Ans1),
    % generate 4 edges per diamond just outside sensor range
    maplist(edges, Input, E),
    flatten(E, Edges),
    sort(Edges, Sorted),
    % only include duplicate lines and separate into positive and negative A
    include([C]>>(select(C,Edges,Rem),memberchk(C,Rem)), Sorted, Duplicates),
    include([CX/_]>>(CX#=(-1)), Duplicates, Neg),
    include([CX/_]>>(CX#=1), Duplicates, Pos),
    % find an intersection point
    [X,Y] ins 1..4000000,
    member(A/B, Neg),
    member(C/D, Pos),
    A*X + B #= C*X + D,
    A*X + B #= Y,
    label([X,Y]),
    maplist(check(X/Y), Input),
    Ans2 #= 4000000 * X + Y,
    write_part2(Ans2).
