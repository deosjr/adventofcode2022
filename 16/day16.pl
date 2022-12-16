:- ['../lib/io.pl'].

:- table part1/4.
:- table part2/5.
:- dynamic transition/3.

%use_test_input.

parse --> eos.
parse --> "Valve ", string_without(" ", C), " has flow rate=", integer(N),
    "; tunnel", opts, "lead", opts, "to valve", opts, parse_valves(Valves), parse,
    {codes_to_atom(C, Valve),
    asserta(flow(Valve, N)),
    maplist([V]>>(codes_to_atom(V,VV), assertz(tunnel(Valve, VV))), Valves)}.
parse_valves([H|T]) --> string_without(",\n", H), ", ", parse_valves(T).
parse_valves([V]) --> string_without(",\n", V), "\n".
opts --> "s ".
opts --> " ".

codes_to_atom(C, A) :-
    atom_codes(AA, C),
    downcase_atom(AA, A).

score(_, Min, 0) :- Min #< 0.
score(Valve, Min, N) :-
    Min #>= 0,
    flow(Valve, F),
    N #= Min * F.

% optimise by saving travel distance between nonzero-flow valves
% then only consider those traversals. stores cost of travel + opening
assert_transitions([], _).
assert_transitions([H|T], Nonzero) :-
    assert_rec(Nonzero, Nonzero, [H], 2, H, H),
    assert_transitions(T, Nonzero).
assert_rec([], _, _, _, _, _, _).
assert_rec(Rest, Nonzero, Visited, Steps, Valve, Pos) :-
    findall(V, tunnel(Pos, V), Connected),
    intersection(Connected, Visited, AlreadyVisited),
    subtract(Connected, AlreadyVisited, NewVisited),
    ( NewVisited = [] -> true ; 
    intersection(Rest, NewVisited, NewVisitedWeCareAbout),
    maplist({Valve,Steps}/[X]>>(transition(Valve,X,Steps)->true;assertz(transition(Valve, X, Steps))), NewVisitedWeCareAbout),
    ( memberchk(Valve, Nonzero) ->
      maplist({Valve,Steps}/[X]>>(transition(X,Valve,Steps)->true;assertz(transition(X, Valve, Steps))), NewVisitedWeCareAbout) ;
      true ),
    Steps1 #= Steps + 1,
    union(Connected, Visited, TotalVisited),
    subtract(Rest, NewVisitedWeCareAbout, LeftToVisit),
    maplist(assert_rec(LeftToVisit, Nonzero, TotalVisited, Steps1, Valve), NewVisited) ).

mappart1([], _, _, []).
mappart1([Valve-Cost|T], Mins, Opened, [Ans|Answers]) :-
    Min #= Mins - Cost,
    score(Valve, Min, Score),
    sort([Valve|Opened], NewOpened),
    part1(Min, NewOpened, Valve, N),
    Ans #= Score + N,
    mappart1(T, Mins, Opened, Answers).

% always move to an unopened valve with flow > 0 and open it!
part1(Mins, _, _, 0) :- Mins #=< 2.
part1(Mins, Opened, Valve, Ans) :-
    Mins #> 2,
    findall(V-Cost, (
        transition(Valve, V, Cost),
        not(memberchk(V, Opened)),
        Cost #< Mins
    ), Connected),
    mappart1(Connected, Mins, Opened, Scores),
    ( Scores = [] -> Ans = 0 ;
      select(Ans, Scores, Rem),
      maplist(#>=(Ans), Rem) ).

% normalise me/elephant so that at least one arrives at destination
normalize((X-Left)-(Y-Left), (X-0)-(Y-0), Left).
normalize((X-XL)-(Y-YL), (X-0)-(Y-NYL), XL) :-
    XL #< YL, NYL #= YL - XL.
normalize((X-XL)-(Y-YL), (X-NXL)-(Y-0), YL) :-
    YL #< XL, NXL #= XL - YL.

connections(From, Mins, Opened, Connected) :-
    findall((V-Cost)/(Score-NewO), (
        transition(From, V, Cost),
        not(memberchk(V, Opened)),
        Cost #< Mins,
        M #= Mins-Cost,
        score(V,M,Score),
        sort([V|Opened],NewO)
    ), Connected).

part2(Mins, _, _, _, 0) :- Mins #=< 2.
% Elephant and I depart at the same time OR I am still underway
% Either way, elphant makes a move as tiebreaker
part2(Mins, Opened, Me-Left, Elephant-0, Ans) :-
    Mins #> 2, Left #>= 0,
    connections(Elephant, Mins, Opened, ConnElephant),
    maplist({Me,Left}/[A/SO,B]>>(B=((Me-Left)-A)/SO), ConnElephant, Connected),
    part2ans(Connected, Mins, Opened, Ans).

% Elephant is still underway, I make a move
part2(Mins, Opened, Me-0, Elephant-Left, Ans) :-
    Mins #> 2, Left #> 0,
    connections(Me, Mins, Opened, ConnMe),
    maplist({Elephant,Left}/[A/SO,B]>>(B=(A-(Elephant-Left))/SO), ConnMe, Connected),
    part2ans(Connected, Mins, Opened, Ans).
    
mappart2([], _, _, []).
mappart2([(Me-Elephant)/(Score-NewOpened)|T], Mins, Opened, [Ans|Answers]) :-
    normalize(Me-Elephant, NewMe-NewElephant, MinsElapsed),
    Min #= Mins - MinsElapsed,
    part2(Min, NewOpened, NewMe, NewElephant, N),
    Ans #= Score + N,
    mappart2(T, Mins, Opened, Answers).

part2ans(Connected, Mins, Opened, Ans) :-
    mappart2(Connected, Mins, Opened, Scores),
    ( Scores = [] -> Ans = 0 ;
      select(Ans, Scores, Rem),
      maplist(#>=(Ans), Rem)
    ).

run :-
    input_stream(16, parse),
    findall(X, flow(X,_), Valves),
    findall(X, (flow(X, N), N#\=0), NonzeroValves),
    assert_transitions(Valves, NonzeroValves),
    part1(30, [], aa, Ans1),
    write_part1(Ans1),
    part2(26, [], aa-0, aa-0, Ans2),
    write_part2(Ans2).
