:- ['../lib/io.pl'].

:- table size/2.

parse(_) --> eos.
parse(Wd) --> parse_command(Wd, NewWd), parse(NewWd).
parse(Wd) --> parse_dir(Wd), parse(Wd).
parse(Wd) --> parse_file(Wd), parse(Wd).

parse_command(Wd, NewWd) --> "$ cd ", string_without("\n", C), "\n",
    {atom_codes(A, C), cd(A, Wd, NewWd)}.
parse_command(Wd, Wd) --> "$ ls", "\n".
parse_dir(Path) --> "dir ", string_without("\n", C), "\n",
    {atom_codes(A, C), assertz(dir(Path, A))}.
parse_file(Path) --> integer(Size), white, string_without("\n", File), "\n",
    {assertz(file(Path, File, Size))}.

cd(.., [_|T], T).
cd(C, T, [C|T]) :-
    \=(C, ..).

ls(Path, Files, Subdirs) :-
    findall(F, file(Path, F, _), Files),
    findall(P, (dir(Path, D), P = [D|Path]), Subdirs).

size(Path, Size) :-
    ls(Path, Files, Subdirs),
    maplist(file(Path), Files, Filesizes),
    maplist(size, Subdirs, Dirsizes),
    sum(Filesizes, #=, FS),
    sum(Dirsizes, #=, DS),
    Size #= FS + DS.

run :-
    input_stream(7, parse([])),
    findall(N, (dir(P, D), size([D|P], N), N #=< 100000), Small),
    sum(Small, #=, Ans1),
    write_part1(Ans1),
    size([/], InUse),
    Free #= 70000000 - InUse,
    Required #= 30000000 - Free,
    findall(N, (dir(P, D), size([D|P], N), N #>= Required), Big),
    maplist(#<(Ans2), Rem),
    select(Ans2, Big, Rem),
    write_part2(Ans2).
