:- use_module(library(pio)).
:- use_module(library(lists)).
:- set_prolog_flag(double_quotes, chars).

:- initialization(main).

main :-
    part1(Part1),
    part2(Part2),
    format('Part 1: ~d~n', [Part1]),
    format('Part 2: ~d~n', [Part2]),
    halt(0).

part1(Ans) :-
    read_file(Tokens),
    maplist(seat_id, Tokens, Ids),
    max_list(Ids, Ans).

part2(Ans) :-
    read_file(Tokens),
    maplist(seat_id, Tokens, Ids),
    my_id(Ids, Ans).

my_id(Ids, Id) :-
    member(Plus, Ids),
    member(Minus, Ids),
    Id is Plus + 1,
    Id is Minus - 1,
    not(member(Id, Ids)).

%% Calculates the Seat ID of a pass (string)
seat_id(PassToken, Id) :-
    parse_pass(PassToken, Pass),
    row_col(Pass, Row, Col),
    Id is Row * 8 + Col.

row_col({Rows, Cols}, Row, Col) :-
    bsp(Rows, 0, 127, Row),
    bsp(Cols, 0, 7, Col).

%% Binary Space Partitioning rules
bsp([0], Lower, _, Lower).
bsp([1], _, Upper, Upper).
bsp([0 | Pass], Lower, Upper, Res) :-
    Half is (Upper + Lower) div 2,
    bsp(Pass, Lower, Half, Res).
bsp([1 | Pass], Lower, Upper, Res) :-
    Half is (Upper + Lower + 1) div 2,
    bsp(Pass, Half, Upper, Res).

%% Parses a token sequence into a compound (pair) of lists of 0s and 1s
parse_pass(Token, {[FB1, FB2, FB3, FB4, FB5, FB6, FB7], LRs}) :-
    atom_chars(Token, Chars),
    maplist(to_binary, Chars, [FB1, FB2, FB3, FB4, FB5, FB6, FB7 | LRs]).

to_binary('F', 0).
to_binary('B', 1).
to_binary('L', 0).
to_binary('R', 1).

%% Reads the file input.txt into a list of tokens
read_file(Tokens) :-
    phrase_from_file(tokens(Tokens), 'input.txt').

%% DCG definitions for parsing
%% Referenced https://stackoverflow.com/a/40189978

token(T) -->
        alnum(L),
        token_(Ls),
        !, % single solution: longest match
        { atom_chars(T, [L|Ls]) }.

alnum(A) --> [A], { char_type(A, alnum) }.

token_([L|Ls]) --> alnum(L), token_(Ls).
token_([])     --> [].

spaces --> [].
spaces --> space, spaces.

space --> [S], { char_type(S, space) }.

tokens([])     --> [].
tokens([T|Ts]) --> token(T), spaces, tokens(Ts).
