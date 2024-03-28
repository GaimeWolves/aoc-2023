string_to_num_list(Str, Ns) :-
    split_string(Str, " ", "", RawStrs),
    exclude(=(""), RawStrs, NumStrs),
    maplist(number_string(), Ns, NumStrs).

parse_line(Input, (Id, Ws, Ns)) :-
    split_string(Input, ":|", "", [IdPart, WinPart, NumPart]),
    string_concat("Card ", IdStr, IdPart),
    string_to_num_list(IdStr, [Id]),
    string_to_num_list(WinPart, Ws),
    string_to_num_list(NumPart, Ns).

get_points([], 0).
get_points(Gs, Pow) :-
    length(Gs, N),
    Pow is 2^(N - 1).

parse_input(Input, Cards) :-
    maplist(parse_line(), Input, Cards).

solve_part_1(Cards, Sum) :-
    maplist([(_, Ws, Ns), Pow]>>(intersection(Ws, Ns, Gs), get_points(Gs, Pow)), Cards, Ps),
    sum_list(Ps, Sum).

add_num_to_n_elems(_, 0, X, X).
add_num_to_n_elems(Num, N, [(Gs, X)|Xs], [(Gs, Y)|Ys]) :-
    Y is X + Num, NewN is N - 1,
    add_num_to_n_elems(Num, NewN, Xs, Ys).

duplicate_cards([],[]).
duplicate_cards([(Gs, N)|Xs], [N|Ns]) :-
    add_num_to_n_elems(N, Gs, Xs, Ys),
    duplicate_cards(Ys, Ns).

solve_part_2(Cards, Sum) :-
    maplist([(_, Ws, Ns), (L, 1)]>>(intersection(Ws, Ns, Gs), length(Gs, L)), Cards, CardPairs),
    duplicate_cards(CardPairs, CardCounts),
    sum_list(CardCounts, Sum).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).
read_lines(Stream, [L|Ls]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_string(Stream, L),
    read_lines(Stream, Ls).

main :-
    open("input.txt", read, Stream),
    read_lines(Stream, Input),
    close(Stream), !,
    parse_input(Input, Cards),
    solve_part_1(Cards, P1),
    write("part 1: "), write(P1), nl,
    solve_part_2(Cards, P2),
    write("part 2: "), write(P2), nl.
    