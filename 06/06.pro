:- use_module(library(dcg/basics)).

int_list([I|Is]) -->
    integer(I),
    int_list_rem(Is).

int_list_rem([]) -->
    eol().
int_list_rem(L) -->
    whites(),
    int_list(L).

input_dcg(Ts, Ds) -->
    `Time:`, whites(), int_list(Ts),
    `Distance:`, whites(), int_list(Ds).

distance(Tm, Ts, D) :-
    D is (Tm - Ts) * Ts.

ways_beaten(Tm, Dr, N) :-
    D is ((Tm / 2)^2 - Dr)^0.5, D > 0,
    N1 is (Tm / 2) - D,
    N2 is (Tm / 2) + D,
    T is ceil(N2 - 1),
    N is ceil(N2 - 1) - floor(N1 + 1) + 1.

solve_part_1(Ts, Ds, Sol) :-
    maplist(ways_beaten(), Ts, Ds, L),
    foldl([A, B, C]>>(C is A * B), L, 1, Sol).

merge(Is, I) :-
    maplist(number_codes(), Is, Istrs),
    flatten(Istrs, Istr),
    number_codes(I, Istr).

solve_part_2(Ts, Ds, Sol) :-
    merge(Ts, Time),
    merge(Ds, Distance),
    ways_beaten(Time, Distance, Sol).

main :-
    phrase_from_file(input_dcg(Ts, Ds), "input.txt"),
    solve_part_1(Ts, Ds, P1),
    write("part 1: "), write(P1), nl, !,
    solve_part_2(Ts, Ds, P2),
    write("part 2: "), write(P2), nl, !.