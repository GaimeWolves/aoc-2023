:- use_module(library(dcg/basics)).

node(S-(L,R)) -->
    string(SStr), ` = (`, string(LStr), `, `, string(RStr), `)`, eol(),
    { atom_codes(S, SStr), atom_codes(L, LStr), atom_codes(R, RStr) }.

node_list([N|Ns]) -->
    node(N), node_list_rem(Ns).

node_list_rem([]) -->
    eol().
node_list_rem(L) --> 
    node_list(L).

input_dcg(Path, Nodes) -->
    string(PathStr), eol(), eol(),
    { maplist([Code, Atom]>>(atom_codes(Atom, [Code])), PathStr, Path) },
    node_list(NodePairs),
    { list_to_assoc(NodePairs, Nodes) }.

get_cyclic(List, Index, Output) :-
    length(List, Length),
    ListIndex is Index mod Length,
    nth0(ListIndex, List, Output).

go(Node, 'L', Nodes, L) :-
    get_assoc(Node, Nodes, (L, _)).
go(Node, 'R', Nodes, R) :-
    get_assoc(Node, Nodes, (_, R)).

search(S, _, _, _, 0, Gs) :-
    member(S, Gs).
search(S, Path, Nodes, Index, Len, Gs) :-
    get_cyclic(Path, Index, Direction),
    go(S, Direction, Nodes, S2),
    Index2 is Index + 1,
    search(S2, Path, Nodes, Index2, Len2, Gs),
    Len is Len2 + 1.

solve_part_1(Path, Nodes, Solution) :-
    search('AAA', Path, Nodes, 0, Solution, ['ZZZ']).

is_start(Key) :-
    atom_codes(Key, [_, _, 65]).

is_end(Key) :-
    atom_codes(Key, [_, _, 90]).

all_starts(Nodes, Starts) :-
    assoc_to_keys(Nodes, Keys),
    include(is_start(), Keys, Starts).

all_ends(Nodes, Ends) :-
    assoc_to_keys(Nodes, Keys),
    include(is_end(), Keys, Ends).

solve_part_2(Path, Nodes, Solution) :-
    all_starts(Nodes, Starts),
    all_ends(Nodes, Ends),
    maplist([S, L]>>(search(S, Path, Nodes, 0, L, Ends)), Starts, Lengths),
    foldl([A, B, C]>>(C is lcm(A, B)), Lengths, 1, Solution).

main :-
    phrase_from_file(input_dcg(Path, Nodes), "input.txt"), !,
    solve_part_1(Path, Nodes, P1),
    write("part 1: "), write(P1), nl, !,
    solve_part_2(Path, Nodes, P2),
    write("part 2: "), write(P2), nl, !.
