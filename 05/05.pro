:- use_module(library(dcg/basics)).

int_list([I|Is]) -->
    integer(I),
    int_list_rem(Is).

int_list_rem([]) -->
    eol().
int_list_rem(L) -->
    white(),
    int_list(L).

seeds(S) -->
    `seeds: `,
    int_list(S).

range((O, I, L)) -->
    integer(O), white(),
    integer(I), white(),
    integer(L), eol().

function([R|Rs]) -->
    range(R),
    function_rem(Rs).

function_rem([]) -->
    eol().
function_rem(L) -->
    function(L).

input_dcg(S, [SS, SF, FW, WL, LT, TH, HL]) -->
    seeds(S), eol(),
    `seed-to-soil map:`,            eol(), function(SS),
    `soil-to-fertilizer map:`,      eol(), function(SF),
    `fertilizer-to-water map:`,     eol(), function(FW),
    `water-to-light map:`,          eol(), function(WL),
    `light-to-temperature map:`,    eol(), function(LT),
    `temperature-to-humidity map:`, eol(), function(TH),
    `humidity-to-location map:`,    eol(), function(HL).

map_value([], X, X).
map_value([(O, I, L)|_], X, Y) :-
    Offset is X - I, Offset >= 0, Offset < L,
    Y is O + Offset.
map_value([_|Rem], X, Y) :-
    map_value(Rem, X, Y).

map_to_final(M, X, Y) :-
    foldl(map_value(), M, X, Y).

solve_part_1(S, M, Sol) :-
    maplist(map_to_final(M), S, L),
    min_list(L, Sol).

to_pairs([], []).
to_pairs([X,Y|R1], [(X,Y)|R2]) :- to_pairs(R1, R2).

is_inside((A, AL), (B, BL)) :-
    AE is A + AL, BE is B + BL,
    A >= B, AE =< BE.

is_valid(R, (A, AL)) :-
    AL > 0, is_inside((A, AL), R).

split((_, I, LF), (S, LR), Ranges) :-
    IE is I + LF, SE is S + LR,
    msort([I, IE, S, SE], [A, B, C, D]),
    L1 is B - A, L2 is C - B, L3 is D - C,
    include(is_valid((S, LR)), [(A, L1), (B, L2), (C, L3)], Ranges).

is_mappable((_, I, LF), (A, AL)) :-
    is_inside((A, AL), (I, LF)).

map((O, I, _), (A, AL), (B, AL)) :-
    Offset is A - I,
    B is O + Offset.

map_ranges(_, [], [], []).
map_ranges(F, [R|Rem], FinalUnmapped, FinalMapped) :-
    map_ranges(F, Rem, UnmappedRest, MappedRest),
    split(F, R, Ranges),
    include(is_mappable(F), Ranges, Mappable),
    exclude(is_mappable(F), Ranges, Unmapped),
    maplist(map(F), Mappable, Mapped),
    append(Unmapped, UnmappedRest, FinalUnmapped),
    append(Mapped, MappedRest, FinalMapped).

apply_map([], Unmapped, Unmapped).
apply_map([F|Fs], Unmapped, Mapped) :-
    map_ranges(F, Unmapped, NewUnmapped, NewMapped),
    apply_map(Fs, NewUnmapped, RemMapped),
    append(NewMapped, RemMapped, Mapped).

solve_part_2(S, M, Sol) :-
    to_pairs(S, SR),
    foldl(apply_map(), M, SR, FR),
    maplist([(A, _), B]>>(A = B), FR, RS),
    min_list(RS, Sol).

main :-
    phrase_from_file(input_dcg(S, M), "input.txt"),
    solve_part_1(S, M, P1),
    write("part 1: "), write(P1), nl, !,
    solve_part_2(S, M, P2),
    write("part 2: "), write(P2), nl, !.