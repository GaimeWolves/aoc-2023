:- use_module(library(dcg/basics)).

hand((Cards, Bid)) -->
    string(Cards), blank(), integer(Bid), eol().

hands([H|Hs]) -->
    hand(H), hands_rem(Hs).

hands_rem([]) -->
    eol().
hands_rem(L) -->
    hands(L).

strength(50, 0).
strength(51, 1).
strength(52, 2).
strength(53, 3).
strength(54, 4).
strength(55, 5).
strength(56, 6).
strength(57, 7).
strength(84, 8).
strength(74, 9).
strength(81, 10).
strength(75, 11).
strength(65, 12).

to_pairs(Cards, Pairs) :-
    msort(Cards, Sorted),
    clumped(Sorted, Pairs).

type([_-5], 6).
type(Pairs, 5) :-
    member(_-4, Pairs).
type(Pairs, 4) :-
    member(_-3, Pairs), member(_-2, Pairs).
type(Pairs, 3) :-
    member(_-3, Pairs).
type(Pairs, 2) :-
    member(A-2, Pairs), member(B-2, Pairs), A \= B.
type(Pairs, 1) :-
    member(_-2, Pairs).
type(_, 0).

power(StrengthGoal, TypeGoal, Cards, Power) :-
    to_pairs(Cards, Pairs),
    call(TypeGoal, Pairs, TypeStrength),
    foldl([C, VI, VO]>>(call(StrengthGoal, C, S), VO is VI * 13 + S), Cards, TypeStrength, Power).

solve(Powers, Solution) :-
    sort(0, @=<, Powers, Sorted), !,
    length(Sorted, N), !,
    numlist(1, N, Mult), !,
    maplist([(_, B1), M, B2]>>(B2 is B1 * M), Sorted, Mult, Multiplied), !,
    sum_list(Multiplied, Solution), !.

solve_part_1(Hands, Solution) :-
    maplist([(C, B1), (P, B2)]>>(B1 = B2, power(strength(), type(), C, P)), Hands, Mapped), !,
    solve(Mapped, Solution), !.

strength2(74, 0).
strength2(50, 1).
strength2(51, 2).
strength2(52, 3).
strength2(53, 4).
strength2(54, 5).
strength2(55, 6).
strength2(56, 7).
strength2(57, 8).
strength2(84, 9).
strength2(81, 10).
strength2(75, 11).
strength2(65, 12).

map_jokers([74-5], [74-5]).
map_jokers(Pairs, Pairs) :-
    not(member(74-_, Pairs)).
map_jokers(Pairs, NewPairs) :-
    select(74-N1, Pairs, Temp),
    append(L, [(T-N2)|R], Temp),
    N is N1 + N2,
    append(L, [(T-N)|R], NewPairs).

type2(Pairs, 6) :-
    map_jokers(Pairs, [_-5]).
type2(Pairs, 5) :-
    map_jokers(Pairs, NewPairs),
    member(_-4, NewPairs).
type2(Pairs, 4) :-
    map_jokers(Pairs, NewPairs),
    member(_-3, NewPairs), member(_-2, NewPairs).
type2(Pairs, 3) :-
    map_jokers(Pairs, NewPairs),
    member(_-3, NewPairs).
type2(Pairs, 2) :-
    map_jokers(Pairs, NewPairs),
    member(A-2, NewPairs), member(B-2, NewPairs), A \= B.
type2(Pairs, 1) :-
    map_jokers(Pairs, NewPairs),
    member(_-2, NewPairs).
type2(_, 0).

solve_part_2(Hands, Solution) :-
    maplist([(C, B1), (P, B2)]>>(B1 = B2, power(strength2(), type2(), C, P)), Hands, Mapped), !,
    solve(Mapped, Solution), !.

main :-
    phrase_from_file(hands(Hs), "input.txt"),
    solve_part_1(Hs, P1),
    write("part 1: "), write(P1), nl, !,
    solve_part_2(Hs, P2),
    write("part 2: "), write(P2), nl, !.
