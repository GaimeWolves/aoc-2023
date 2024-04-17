width([Row|_], Width) :-
    length(Row, Width).

at(Grid, (X, Y), Code) :-
    is_valid(Grid, (X, Y)),
    nth0(Y, Grid, Row),
    nth0(X, Row, Code).

index_of_start(Grid, (X, Y)) :-
    include([Row]>>nth0(_, Row, 83), Grid, [Row]),
    nth0(Y, Grid, Row),
    nth0(X, Row, 83).

is_valid([Row|Rest], (X, Y)) :-
    length(Row, Width),
    length([Row|Rest], Height),
    X >= 0, Y >= 0,
    X < Width, Y < Height.

is_start_of_pipe(Grid, (X, Y), (X2, Y)) :-
    X2 is X + 1,
    at(Grid, (X2, Y), Code),
    member(Code, [45, 74, 55, 83]).
is_start_of_pipe(Grid, (X, Y), (X2, Y)) :-
    X2 is X - 1,
    at(Grid, (X2, Y), Code),
    member(Code, [45, 70, 76, 83]).
is_start_of_pipe(Grid, (X, Y), (X, Y2)) :-
    Y2 is Y - 1,
    at(Grid, (X, Y2), Code),
    member(Code, [124, 70, 55, 83]).
is_start_of_pipe(Grid, (X, Y), (X, Y2)) :-
    Y2 is Y + 1,
    at(Grid, (X, Y2), Code),
    member(Code, [124, 74, 76, 83]).

neighbor((X, Y), (X2, Y)) :- X2 is X - 1.
neighbor((X, Y), (X2, Y)) :- X2 is X + 1.
neighbor((X, Y), (X, Y2)) :- Y2 is Y - 1.
neighbor((X, Y), (X, Y2)) :- Y2 is Y + 1.

connected_to(Grid, (X, Y), [A,B]) :-
    findall((X2, Y2), (neighbor((X, Y), (X2, Y2)), is_start_of_pipe(Grid, (X, Y), (X2, Y2))), [A, B]).

connections(Grid, (X, Y), [(X2, Y2), (X3, Y3)]) :-
    at(Grid, (X, Y), 45),
    X2 is X - 1, Y2 is Y,
    X3 is X + 1, Y3 is Y.
connections(Grid, (X, Y), [(X2, Y2), (X3, Y3)]) :-
    at(Grid, (X, Y), 55),
    X2 is X - 1, Y2 is Y,
    X3 is X,     Y3 is Y + 1.
connections(Grid, (X, Y), [(X2, Y2), (X3, Y3)]) :-
    at(Grid, (X, Y), 70),
    X2 is X + 1, Y2 is Y,
    X3 is X,     Y3 is Y + 1.
connections(Grid, (X, Y), [(X2, Y2), (X3, Y3)]) :-
    at(Grid, (X, Y), 74),
    X2 is X - 1, Y2 is Y,
    X3 is X,     Y3 is Y - 1.
connections(Grid, (X, Y), [(X2, Y2), (X3, Y3)]) :-
    at(Grid, (X, Y), 76),
    X2 is X + 1, Y2 is Y,
    X3 is X,     Y3 is Y - 1.
connections(Grid, (X, Y), [(X2, Y2), (X3, Y3)]) :-
    at(Grid, (X, Y), 124),
    X2 is X,     Y2 is Y - 1,
    X3 is X,     Y3 is Y + 1.

find_path(_, _, P, P, [P]).
find_path(Grid, Visited, P1, P2, [P1|Path]) :-
    connections(Grid, P1, [P, _]),
    not(member(P, Visited)),
    find_path(Grid, [P1|Visited], P, P2, Path).
find_path(Grid, Visited, P1, P2, [P1|Path]) :-
    connections(Grid, P1, [_, P]),
    not(member(P, Visited)),
    find_path(Grid, [P1|Visited], P, P2, Path).

start_cycle(Grid, [Start|Path]) :-
    index_of_start(Grid, Start),
    connected_to(Grid, Start, [P1, P2]),
    find_path(Grid, [Start], P1, P2, Path).

raycast(Grid, _, (X, _), _, _, 0) :-
    width(Grid, X).
raycast(Grid, Path, (X, Y), State, N, Ns) :- % '|' and 'S' on path
    member((X, Y), Path),
    at(Grid, (X, Y), Code),
    member(Code, [83, 124]),
    X2 is X + 1,
    N2 is N + 1,
    raycast(Grid, Path, (X2, Y), State, N2, Ns).
raycast(Grid, Path, (X, Y), _, N, Ns) :- % 'L' on path
    member((X, Y), Path),
    at(Grid, (X, Y), 76),
    X2 is X + 1,
    raycast(Grid, Path, (X2, Y), l, N, Ns).
raycast(Grid, Path, (X, Y), _, N, Ns) :- % 'F' on path
    member((X, Y), Path),
    at(Grid, (X, Y), 70),
    X2 is X + 1,
    raycast(Grid, Path, (X2, Y), f, N, Ns).
raycast(Grid, Path, (X, Y), f, N, Ns) :- % 'J' on path with previous 'F'
    member((X, Y), Path),
    at(Grid, (X, Y), 74),
    X2 is X + 1,
    N2 is N + 1,
    raycast(Grid, Path, (X2, Y), off, N2, Ns).
raycast(Grid, Path, (X, Y), l, N, Ns) :- % '7' on path with previous 'L'
    member((X, Y), Path),
    at(Grid, (X, Y), 55),
    X2 is X + 1,
    N2 is N + 1,
    raycast(Grid, Path, (X2, Y), off, N2, Ns).
raycast(Grid, Path, (X, Y), State, N, Ns) :- % on path
    member((X, Y), Path),
    X2 is X + 1,
    raycast(Grid, Path, (X2, Y), State, N, Ns).
raycast(Grid, Path, (X, Y), State, N, Ns) :- % not on path and not inside
    N mod 2 =:= 0,
    X2 is X + 1,
    raycast(Grid, Path, (X2, Y), State, N, Ns).
raycast(Grid, Path, (X, Y), State, N, NewNs) :- % not on path and inside
    X2 is X + 1,
    raycast(Grid, Path, (X2, Y), State, N, Ns),
    NewNs is Ns + 1.

raycast_line(Grid, Path, Y, Ns) :-
    raycast(Grid, Path, (0, Y), off, 0, Ns).

raycast_grid(Grid, Path, Counts) :-
    length(Grid, Height),
    MaxY is Height - 1,
    numlist(0, MaxY, Ys),
    maplist(raycast_line(Grid, Path), Ys, Counts).

solve(Grid, P1, P2) :-
    start_cycle(Grid, Path),
    length(Path, N),
    P1 is N // 2,
    raycast_grid(Grid, Path, Counts),
    sum_list(Counts, P2).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).
read_lines(Stream, [L|Ls]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_codes(Stream, L),
    read_lines(Stream, Ls).

main :-
    open("input.txt", read, Stream),
    read_lines(Stream, Grid),
    close(Stream),
    solve(Grid, P1, P2),
    write("part 1: "), write(P1), nl,
    write("part 2: "), write(P2), nl.
