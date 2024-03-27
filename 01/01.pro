digit(`one`, 1).
digit(`two`, 2).
digit(`three`, 3).
digit(`four`, 4).
digit(`five`, 5).
digit(`six`, 6).
digit(`seven`, 7).
digit(`eight`, 8).
digit(`nine`, 9).

first_digit(Xs, Y, normal, part2) :-
    digit(Ds, Y),
    append(Ds, _, Xs).
first_digit(Xs, Y, reverse, part2) :-
    digit(Ds, Y),
    reverse(Ds, RevDs),
    append(RevDs, _, Xs).
first_digit([X|_], Y, _, _) :-
    X >= 48, X =< 57,
    Y is X - 48.
first_digit([_|Xs], X, M, P) :-
    first_digit(Xs, X, M, P).

calibration_value(S, Z, Part) :-
    first_digit(S, X, normal, Part),
    reverse(S, RevS),
    first_digit(RevS, Y, reverse, Part),
    Z is 10 * X + Y.

solve([], 0, _).
solve([S|Ss], Output, Part) :-
    calibration_value(S, X, Part),
    solve(Ss, Y, Part),
    Output is X + Y.

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
    solve(Input, P1, part1),
    write("part 1: "), write(P1), nl,
    solve(Input, P2, part2),
    write("part 2: "), write(P2), nl.
