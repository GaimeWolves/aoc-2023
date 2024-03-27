match_parts(Game, Id, RoundParts) :-
    re_matchsub("Game (\\d+): (.*)", Game, Matches),
    number_chars(Id, Matches.1),
    split_string(Matches.2, ";", "", RoundParts).

get_number(RoundPart, Regex, 0) :-
    not(re_matchsub(Regex, RoundPart, _)).
get_number(RoundPart, Regex, Num) :-
    re_matchsub(Regex, RoundPart, Match),
    number_chars(Num, Match.1).

get_round(RoundPart, Rolls) :-
    get_number(RoundPart, ".*?(\\d+) red.*", RNum),
    get_number(RoundPart, ".*?(\\d+) green.*", GNum),
    get_number(RoundPart, ".*?(\\d+) blue.*", BNum),
    Rolls = _{r: RNum, g: GNum, b: BNum}.

get_rounds([], []).
get_rounds([Part|PartRest], [Round|RoundRest]) :-
    get_round(Part, Round),
    get_rounds(PartRest, RoundRest).

get_game(Game, _{id: ID, rs: Rounds}) :-
    match_parts(Game, ID, RoundParts),
    get_rounds(RoundParts, Rounds).

get_games([], []).
get_games([GameStr|GameStrRest], [Game|GameRest]) :-
    get_game(GameStr, Game),
    get_games(GameStrRest, GameRest).

is_round_valid(Round) :-
    Round.r =< 12,
    Round.g =< 13,
    Round.b =< 14.

all_rounds_valid([]).
all_rounds_valid([Round|Rest]) :-
    is_round_valid(Round),
    all_rounds_valid(Rest).

filter_valid_games([], []).
filter_valid_games([Game|Rest], [Game|ValidRest]) :-
    all_rounds_valid(Game.rs),
    filter_valid_games(Rest, ValidRest).
filter_valid_games([Game|Rest], Valid) :-
    not(all_rounds_valid(Game.rs)),
    filter_valid_games(Rest, Valid).

sum_ids([], 0).
sum_ids([Game|Rest], N) :-
    sum_ids(Rest, OldN),
    N is OldN + Game.id.

solve_part_1(Input, Output) :-
    filter_valid_games(Input, Valid),
    sum_ids(Valid, Output).

max_round([], _{r: 0, b: 0, g: 0}).
max_round([_{r: R1, b: B1, g: G1}|Rest], _{r: MaxR, b: MaxB, g: MaxG}) :-
    max_round(Rest, _{r: R2, b: B2, g: G2}),
    MaxR = max(R1, R2),
    MaxB = max(B1, B2),
    MaxG = max(G1, G2).

sum_powers([], 0).
sum_powers([Game|Rest], SumPowers) :-
    sum_powers(Rest, SumRest),
    max_round(Game.rs, _{r: R, b: B, g: G}),
    SumPowers is SumRest + (R * G * B).

solve_part_2(Input, Output) :-
    sum_powers(Input, Output).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).
read_lines(Stream, [L|Ls]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_string(Stream, L),
    read_lines(Stream, Ls).

main :-
    open("input.txt", read, Stream),
    read_lines(Stream, Input),
    close(Stream),
    get_games(Input, Games),
    solve_part_1(Games, P1),
    write("part 1: "), write(P1), nl,
    solve_part_2(Games, P2),
    write("part 2: "), write(P2), nl.
