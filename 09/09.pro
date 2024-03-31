:- use_module(library(dcg/basics)).

int_list([I|Is]) -->
    integer(I),
    int_list_rem(Is).

int_list_rem([]) -->
    eol().
int_list_rem(L) -->
    whites(),
    int_list(L).

readings_list([R|Rs]) -->
    int_list(R),
    readings_list_rem(Rs).

readings_list_rem([]) -->
    eos().
readings_list_rem(Rs) -->
    readings_list(Rs).

build_difference_list([_], []).
build_difference_list([X,Y|Xs], [D|Ds]) :-
    D is Y - X,
    build_difference_list([Y|Xs], Ds).

build_tree(Seq, []) :-
    maplist(=(0), Seq).
build_tree(Seq, [Seq|Rem]) :-
    build_difference_list(Seq, DiffList),
    build_tree(DiffList, Rem).

extrapolate_forwards([], 0).
extrapolate_forwards([DiffList|Rem], Value) :-
    last(DiffList, Last),
    extrapolate_forwards(Rem, Above),
    Value is Last + Above.

extrapolate_backwards([], 0).
extrapolate_backwards([[First|_]|Rem], Value) :-
    extrapolate_backwards(Rem, Above),
    Value is First - Above.

solve_part_1(Rs, Solution) :-
    maplist(build_tree(), Rs, Ds),
    maplist(extrapolate_forwards(), Ds, Vs),
    sum_list(Vs, Solution).

solve_part_2(Rs, Solution) :-
    maplist(build_tree(), Rs, Ds),
    maplist(extrapolate_backwards(), Ds, Vs),
    sum_list(Vs, Solution).

main :-
    phrase_from_file(readings_list(Rs), "input.txt"),
    solve_part_1(Rs, P1),
    write("part 1: "), write(P1), nl, !,
    solve_part_2(Rs, P2),
    write("part 2: "), write(P2), nl, !.
