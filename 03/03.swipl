digit(C) :-
    C >= 48, C =< 57.

dot(46).

dir(-1, -1).
dir(-1, 0).
dir(-1, 1).
dir(0, -1).
dir(0, 1).
dir(1, -1).
dir(1, 0).
dir(1, 1).

width([Row|_], X) :- length(Row, X).
height(Rows, Y) :- length(Rows, Y).

is_valid(Input, X, Y) :-
    width(Input, Mx),
    height(Input, My),
    X >= 0, X < Mx,
    Y >= 0, Y < My.

at(Input, X, Y, E) :-
    is_valid(Input, X, Y),
    nth0(Y, Input, Row),
    nth0(X, Row, E).

neighbour(Input, X, Y, E, Nx, Ny) :-
    dir(Dx, Dy),
    Nx is X + Dx, Ny is Y + Dy,
    at(Input, Nx, Ny, E).

neighbour2(Input, X, Y, _{e: E, x: Nx, y: Ny}) :-
    dir(Dx, Dy),
    Nx is X + Dx, Ny is Y + Dy,
    at(Input, Nx, Ny, E).

is_part_number(Input, X, Y, _) :-
    at(Input, X, Y, E), digit(E),
    neighbour(Input, X, Y, N, _, _),
    not(digit(N)), not(dot(N)), !.
is_part_number(Input, X, Y, right) :-
    at(Input, X, Y, E), digit(E),
    Nx is X + 1,
    at(Input, Nx, Y, N),
    digit(N),
    is_part_number(Input, Nx, Y, right).
is_part_number(Input, X, Y, left) :-
    at(Input, X, Y, E), digit(E),
    Nx is X - 1,
    at(Input, Nx, Y, N),
    digit(N),
    is_part_number(Input, Nx, Y, left).

filter_part_numbers_helper(Input, X, Y, [], []) :-
    height(Input, My), Y =:= My - 1,
    width(Input, Mx), X >= Mx.
filter_part_numbers_helper(Input, X, Y, [], [Row|Output]) :-
    height(Input, My), Y < My - 1,
    width(Input, Mx), X >= Mx,
    Ny is Y + 1,
    filter_part_numbers_helper(Input, 0, Ny, Row, Output).
filter_part_numbers_helper(Input, X, Y, [32|Row], Output) :-
    is_valid(Input, X, Y),
    not(is_part_number(Input, X, Y, _)),
    Nx is X + 1,
    filter_part_numbers_helper(Input, Nx, Y, Row, Output).
filter_part_numbers_helper(Input, X, Y, [E|Row], Output) :-
    is_valid(Input, X, Y),
    is_part_number(Input, X, Y, _),
    Nx is X + 1,
    filter_part_numbers_helper(Input, Nx, Y, Row, Output),
    at(Input, X, Y, E).

filter_part_numbers(Input, [Row|Output]) :-
    filter_part_numbers_helper(Input, 0, 0, Row, Output).

filter_to_numbers([], []).
filter_to_numbers([""|A], B) :-
    filter_to_numbers(A, B).
filter_to_numbers([S|A], [N|B]) :-
    number_string(N, S),
    filter_to_numbers(A, B).

filter_rows_to_numbers([], []).
filter_rows_to_numbers([Row|GridRest], AllNums) :-
    string_codes(S, Row),
    split_string(S, " ", "", L),
    filter_to_numbers(L, Nums),
    filter_rows_to_numbers(GridRest, NumsRest),
    append(Nums, NumsRest, AllNums).

solve_part_1(Input, Output) :-
    filter_part_numbers(Input, Temp),
    filter_rows_to_numbers(Temp, Nums),
    sum_list(Nums, Output).

get_start_of_number(Input, 0, Y, 0) :-
    at(Input, 0, Y, E),
    digit(E).
get_start_of_number(Input, X, Y, X) :-
    at(Input, X, Y, E),
    digit(E),
    Nx is X - 1,
    at(Input, Nx, Y, N),
    not(digit(N)).
get_start_of_number(Input, X, Y, Sx) :-
    at(Input, X, Y, E),
    digit(E),
    Nx is X - 1,
    get_start_of_number(Input, Nx, Y, Sx).

filtermap_to_start_of_number(_, [], []).
filtermap_to_start_of_number(Input, [_{e: E, x: _, y: _}|Rest], Starts) :-
    not(digit(E)),
    filtermap_to_start_of_number(Input, Rest, Starts).
filtermap_to_start_of_number(Input, [_{e: E, x: X, y: Y}|Rest], [_{x: Sx, y: Y}|RestStarts]) :-
    digit(E),
    get_start_of_number(Input, X, Y, Sx),
    filtermap_to_start_of_number(Input, Rest, RestStarts).

to_set([], []).
to_set([X|Xs], [X|Ss]) :-
    to_set(Xs, Ss),
    not(member(X, Ss)).
to_set([X|Xs], S) :-
    to_set(Xs, S),
    member(X, Xs).

number_length([], 0).
number_length([C|_], 0) :-
    not(digit(C)).
number_length([C|Cs], N) :-
    digit(C),
    number_length(Cs, N2),
    N is N2 + 1.

get_number_at(Input, X, Y, N) :-
    nth0(Y, Input, Row),
    length(Prefix, X),
    append(Prefix, Suffix, Row),
    number_length(Suffix, NumLen),
    length(NumStr, NumLen),
    prefix(NumStr, Suffix),
    number_codes(N, NumStr).

get_gear_ratio_at(Input, X, Y, Ratio) :-
    at(Input, X, Y, 42),
    findall(N, (neighbour2(Input, X, Y, N)), Ns),
    filtermap_to_start_of_number(Input, Ns, Candidates),
    to_set(Candidates, [_{x: X1, y: Y1}, _{x: X2, y: Y2}]),
    get_number_at(Input, X1, Y1, N1),
    get_number_at(Input, X2, Y2, N2),
    Ratio is N1 * N2.

filter_gear_ratios(Input, X, Y, []) :-
    height(Input, My), Y =:= My - 1,
    width(Input, Mx), X >= Mx, !.
filter_gear_ratios(Input, X, Y, Output) :-
    height(Input, My), Y < My - 1,
    width(Input, Mx), X >= Mx,
    Ny is Y + 1, !,
    filter_gear_ratios(Input, 0, Ny, Output).
filter_gear_ratios(Input, X, Y, [Ratio|Output]) :-
    get_gear_ratio_at(Input, X, Y, Ratio),
    Nx is X + 1,
    filter_gear_ratios(Input, Nx, Y, Output).
filter_gear_ratios(Input, X, Y, Output) :-
    not(get_gear_ratio_at(Input, X, Y, _)),
    Nx is X + 1,
    filter_gear_ratios(Input, Nx, Y, Output).

solve_part_2(Input, Output) :-
    filter_gear_ratios(Input, 0, 0, Nums),
    sum_list(Nums, Output).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).
read_lines(Stream, [L|Ls]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_codes(Stream, L),
    read_lines(Stream, Ls).

main :-
    open("input.txt", read, Stream),
    read_lines(Stream, Input),
    close(Stream),
    solve_part_1(Input, P1),
    write("part 1: "), write(P1), nl,
    solve_part_2(Input, P2),
    write("part 2: "), write(P2), nl.
